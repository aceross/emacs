// JSON-lines adapter for the editor tools shipped with the selected Quarto.
import readline from 'node:readline';
import {createRequire} from 'node:module';
import {pathToFileURL} from 'node:url';
import {resolve} from 'node:path';
const require = createRequire(import.meta.url);
const YAML = require('yaml');
console.log = (...args) => console.error(...args);
let bundlePromise;
async function editorBundle() {
  bundlePromise ??= import(pathToFileURL(process.argv[2])).then(bundle => {
    for (const name of ['getCompletions', 'getHover', 'getLint']) {
      if (typeof bundle[name] !== 'function') throw Error(`Quarto editor bundle lacks ${name}`);
    }
    return bundle;
  });
  return bundlePromise;
}

// Keep editor context consistent with unsaved metadata, without running Quarto
// or any document code. Incomplete YAML is expected while someone is typing.
function prepareContext(context, method) {
  const result = {...context};
  let metadata;
  try {
    metadata = YAML.parseDocument(context.metadata || '', {strict: false}).toJS();
  } catch {
    // A broken alias or partial value must not prevent Quarto from linting
    // the original document. Retain the last resolved engine/format instead.
  }
  if (metadata && typeof metadata === 'object' && !Array.isArray(metadata)) {
    result.engine = metadata.engine || (metadata.jupyter ? 'jupyter' :
      metadata.knitr ? 'knitr' : result.engine);
    if (!context.selected_format && metadata.format) {
      result.formats = typeof metadata.format === 'string'
        ? [metadata.format] : Object.keys(metadata.format);
    }
  }
  delete result.metadata;
  if (method !== 'getLint' && context.front_matter_open) {
    result.code += '\n---\n';
  }
  if (method === 'getCompletions') {
    const lines = result.code.split('\n');
    let line = lines[context.position.row].slice(0, context.position.column);
    // Quarto 1.4 otherwise completes the value at the end of a line even when
    // the cursor is in its key. Query a shadow line ending at the cursor.
    // Quotes remain in the source; querying an unquoted partial scalar also
    // lets the schema complete a value inside an unfinished quoted string.
    line = line.replace(/(:\s*)["']([^"']*)$/, '$1$2');
    lines[context.position.row] = line;
    result.code = lines.join('\n');
    result.line = line;
    result.position = {...context.position, column: line.length};
  }
  return result;
}

function compactCompletions(result) {
  if (!result || !Array.isArray(result.completions)) {
    throw Error('Quarto editor returned an incompatible completion response; see the editor log');
  }
  return {
    token: result.token,
    cacheable: result.cacheable,
    completions: result.completions.map(item => ({
      value: item.value, type: item.type,
      description: item.description || item.documentation || '',
      suggest_on_accept: !!item.suggest_on_accept,
    })),
  };
}

const input = readline.createInterface({input: process.stdin});
for await (const line of input) {
  let request;
  try {
    request = JSON.parse(line);
    let value = request.method === 'bibliography'
      ? (() => {
          // Resolve this one authoring setting from current metadata. An old
          // saved document override must not survive its deletion in the buffer.
          const bibliography = [];
          const collect = (object, directory) => {
            for (const value of [object?.bibliography, object?.format?.[request.format]?.bibliography]) {
              if (value == null) continue;
              const files = typeof value === 'string' ? [value] : value;
              if (!(Array.isArray(files) && files.every(item => typeof item === 'string'))) {
                throw Error('bibliography must be a filename or a list of filenames');
              }
              // Quarto 1.4 combines inherited bibliography lists.
              bibliography.push(...files.map(file => resolve(directory, file)));
            }
          };
          collect(request.project, request.project_directory);
          for (const item of request.directory_metadata || []) {
            collect(YAML.parse(item.code || '') || {}, item.directory);
          }
          collect(YAML.parse(request.code || '') || {}, request.document_directory);
          return {bibliography: [...new Set(bibliography)]};
        })()
      : request.method === 'parse'
      ? YAML.parse(request.code)
      : await (await editorBundle())[request.method](prepareContext(request.context, request.method));
    if (request.method === 'getCompletions') value = compactCompletions(value);
    process.stdout.write(JSON.stringify({id: request.id, value}) + '\n');
  } catch (error) {
    process.stdout.write(JSON.stringify({id: request?.id, error: String(error)}) + '\n');
  }
}
