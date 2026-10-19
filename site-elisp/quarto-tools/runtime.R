# Document-owned knitr runtime. R starts in the project before this file runs.
# SIGINT may arrive immediately after Emacs dispatches a request, before R
# enters evaluate(). Keep protocol reads/writes intact and deliver a pending
# interrupt only inside the request's installed handler.
local(suspendInterrupts({
  for (package in c("jsonlite", "evaluate", "knitr")) {
    if (!requireNamespace(package, quietly = TRUE))
      stop(sprintf("Install %s explicitly in this project's R library", package))
  }
  args <- commandArgs(trailingOnly = TRUE)
  artefacts <- normalizePath(args[[1]], mustWork = TRUE)
  protocol <- file("/dev/stdout", open = "w", encoding = "UTF-8")
  input <- file("stdin", open = "r")
  state <- .GlobalEnv
  request <- NULL
  plot_number <- 0L
  failed <- FALSE
  emit <- function(type, ...) {
    event <- c(list(type = type, id = request$id, document = request$document,
                    chunk = request$chunk, revision = request$revision), list(...))
    writeLines(paste0("\036QUARTO", jsonlite::toJSON(event, auto_unbox = TRUE,
                                                   null = "null", na = "null")), protocol)
    flush(protocol)
    invisible(NULL)
  }
  plot <- function(value) {
    plot_number <<- plot_number + 1L
    path <- file.path(artefacts, sprintf("plot-%06d.png", plot_number))
    grDevices::png(path, width = 1000, height = 700, res = 120)
    tryCatch(grDevices::replayPlot(value), finally = grDevices::dev.off())
    emit("plot", path = path)
  }
  handler <- evaluate::new_output_handler(
    source = function(x) NULL,
    text = function(x) emit("text", text = x),
    graphics = plot,
    message = function(x) emit("message", text = conditionMessage(x)),
    warning = function(x) emit("warning", text = conditionMessage(x)),
    error = function(x) { failed <<- TRUE; emit("error", text = conditionMessage(x)) }
  )
  python_output <- function(outputs, options) {
    for (output in outputs) {
      if (inherits(output, "source")) next
      if (inherits(output, "recordedplot")) plot(output)
      else if (inherits(output, "knit_image_paths") || inherits(output, "knit_image")) {
        for (path in as.character(output)) emit("plot", path = normalizePath(path))
      } else if (is.character(output)) emit("text", text = paste(output, collapse = "\n"))
      else emit("text", text = paste(capture.output(print(output)), collapse = "\n"))
    }
    invisible(NULL)
  }
  options(reticulate.engine.wrap = python_output, knitr.in.progress = TRUE)
  knitr::opts_knit$set(global.device = FALSE, output.dir = artefacts, base.dir = artefacts)
  knitr::knit_global(state)
  emit("ready", engine = "knitr", r = R.version.string,
       startup_directory = getwd(), libraries = .libPaths(), python = Sys.getenv("RETICULATE_PYTHON"))
  repeat {
    line <- tryCatch(readLines(input, n = 1L, warn = FALSE), interrupt = function(e) "")
    if (!length(line)) break
    if (!nzchar(line)) next
    request <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(request)) next
    if (identical(request$method, "shutdown")) break
    failed <- FALSE
    status <- "succeeded"
    emit("running")
    tryCatch(allowInterrupts({
      setwd(request$directory)
      if (identical(request$method, "table")) {
        name <- request$name
        offset <- max(0L, as.integer(request$offset))
        limit <- min(200L, max(1L, as.integer(request$limit)))
        if (identical(request$language, "python")) {
          if (!requireNamespace("reticulate", quietly = TRUE)) stop("Install reticulate explicitly")
          if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", name)) stop("Use a data frame variable name")
          object <- reticulate::py_eval(name, convert = FALSE)
          total <- as.integer(reticulate::py_to_r(object$shape[[0L]]))
          # iloc slicing remains inside Python; only this page is converted.
          page <- reticulate::py_to_r(object$iloc$`__getitem__`(
            reticulate::import_builtins()$slice(offset, min(total, offset + limit))))
        } else {
          object <- get(name, envir = state, inherits = FALSE)
          if (!is.data.frame(object)) stop("The named R object is not a data frame")
          total <- nrow(object)
          indices <- if (offset >= total) integer() else seq.int(offset + 1L, min(total, offset + limit))
          page <- object[indices, , drop = FALSE]
        }
        emit("table", columns = I(names(page)), rows = unname(lapply(seq_len(nrow(page)), function(i)
          unname(lapply(page, function(column) as.character(column[[i]]))))), total = total, offset = offset)
      } else if (identical(request$language, "r")) {
        evaluate::evaluate(request$code, envir = state, output_handler = handler,
                           stop_on_error = 1L, filename = request$file)
      } else if (identical(request$language, "python")) {
        if (!requireNamespace("reticulate", quietly = TRUE)) stop("Install reticulate explicitly")
        opts <- knitr::opts_chunk$merge(list(
          code = strsplit(request$code, "\n", fixed = TRUE)[[1]], engine = "python",
          label = paste0("interactive-", request$id), eval = TRUE, echo = FALSE,
          include = TRUE, warning = TRUE, message = TRUE, error = FALSE,
          fig.path = paste0(artefacts, "/"), dev = "png", fig.width = 7, fig.height = 5))
        knitr::opts_current$set(opts)
        reticulate::eng_python(opts)
      } else stop("Unsupported language for knitr")
      if (failed) status <- "failed"
    }), interrupt = function(e) { status <<- "interrupted"; emit("warning", text = "Interrupted") },
       error = function(e) { status <<- "failed"; emit("error", text = conditionMessage(e)) })
    emit("done", status = status)
  }
  close(input)
  close(protocol)
}))
