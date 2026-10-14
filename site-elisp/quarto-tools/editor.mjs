// JSON-lines adapter for the editor tools shipped with the selected Quarto.
import readline from 'node:readline';
import {createRequire} from 'node:module';
import {pathToFileURL} from 'node:url';
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
const input = readline.createInterface({input: process.stdin});
for await (const line of input) {
  let request;
  try {
    request = JSON.parse(line);
    const value = request.method === 'parse'
      ? YAML.parse(request.code)
      : await (await editorBundle())[request.method](request.context);
    process.stdout.write(JSON.stringify({id: request.id, value}) + '\n');
  } catch (error) {
    process.stdout.write(JSON.stringify({id: request?.id, error: String(error)}) + '\n');
  }
}
