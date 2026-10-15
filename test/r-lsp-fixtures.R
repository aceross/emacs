# Prepare ONE disposable environment. Never run against a research project.
# Rscript --vanilla test/r-lsp-fixtures.R alpha (then beta)
# Requires languageserver in a normal R library and renv in base/R-library.
base <- Sys.getenv("AWC_R_LSP_TEST_DIR", unset = "/tmp/awc-r-lsp-check")
stopifnot(startsWith(normalizePath(base, mustWork = TRUE), "/tmp/"))
Sys.setenv(RENV_PATHS_ROOT = file.path(base, "cache"),
           RENV_CONFIG_SANDBOX_ENABLED = "FALSE")
.libPaths(c(file.path(base, "R-library"), .libPaths()))
packages <- installed.packages()
needed <- unique(c("languageserver", tools::package_dependencies(
  "languageserver", packages, recursive = TRUE)[[1L]]))
label <- commandArgs(TRUE)[[1L]]
stopifnot(label %in% c("alpha", "beta"))
root <- file.path(base, paste("project", label))
if (dir.exists(root)) stop("Refusing to overwrite an existing fixture: ", root)
dir.create(file.path(root, "reports", "chapters"), recursive = TRUE, showWarnings = FALSE)
writeLines("Version: 1.0", file.path(root, "fixture.Rproj"))
writeLines("project:\n  type: default", file.path(root, "reports", "_quarto.yml"))
renv::init(project = root, bare = TRUE, restart = FALSE)
lib <- renv::paths$library(project = root)
for (pkg in needed) {
  if (is.na(packages[pkg, "Priority"])) {
    target <- file.path(lib, pkg)
    if (!file.exists(target)) file.symlink(file.path(packages[pkg, "LibPath"], pkg), target)
  }
}
writeLines(c(paste0("AWC_TEST_PROJECT=", label),
             paste0("RENV_PATHS_ROOT=", file.path(base, "cache")),
             "RENV_CONFIG_SANDBOX_ENABLED=FALSE"), file.path(root, ".Renviron"))
cat(sprintf('\noptions(awc.test.project = "%s")\nwriteLines(c(getwd(), .libPaths(), Sys.getenv("AWC_TEST_PROJECT"), Sys.getenv("RETICULATE_PYTHON")), sprintf("startup-%%s.txt", Sys.getpid()))\n', label),
    file = file.path(root, ".Rprofile"), append = TRUE)
writeLines("linters: list(assignment_linter())", file.path(root, ".lintr"))
pkgdir <- file.path(base, paste0("envprobe-", label))
dir.create(file.path(pkgdir, "R"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(pkgdir, "man"), showWarnings = FALSE)
fun <- paste0(label, "_only")
writeLines(c("Package: envprobe", paste("Version:", if (label == "alpha") "1.0.0" else "2.0.0"),
             "Title: Isolated Test Environment", "Description: Temporary environment identity probe.",
             "Authors@R: person('Test', 'Fixture', email='test@example.org', role=c('aut','cre'))",
             "License: MIT"), file.path(pkgdir, "DESCRIPTION"))
writeLines(paste0("export(", fun, ")"), file.path(pkgdir, "NAMESPACE"))
writeLines(sprintf('%s <- function(%s_value = 1) "%s"', fun, label, label), file.path(pkgdir, "R", "probe.R"))
writeLines(sprintf('\\name{%s}\n\\alias{%s}\n\\title{%s environment probe}\n\\usage{%s(%s_value = 1)}\n\\arguments{\\item{%s_value}{A test value.}}\n\\description{Only available in %s.}',
                   fun, fun, label, fun, label, label, label), file.path(pkgdir, "man", "probe.Rd"))
status <- system2(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", "--no-multiarch",
                 shQuote(paste0("--library=", lib)), shQuote(pkgdir)))
stopifnot(status == 0)
writeLines(c("library(envprobe)", "project_calculate <- function(value) {",
             paste("  value +", if (label == "alpha") "1" else "2"), "}",
             "result <- project_calculate(2)", paste0("envprobe::", fun, "(3)"), "bad = 1"),
           file.path(root, "reports", "chapters", "analysis.R"))
writeLines(lib, file.path(base, paste0(label, "-library.txt")))
cat("READY", root, lib, "\n")
status <- system2(Sys.getenv("QUARTO_TEST_PYTHON", unset = "python3"), c("-m", "venv", "--without-pip", shQuote(file.path(root, ".venv"))))
stopifnot(status == 0)
missing <- file.path(base, "missing package")
if (!dir.exists(missing)) {
  dir.create(missing)
  writeLines("Version: 1.0", file.path(missing, "fixture.Rproj"))
  writeLines(".libPaths(.Library, include.site = FALSE)", file.path(missing, ".Rprofile"))
  writeLines("answer <- 42", file.path(missing, "analysis.R"))
}
