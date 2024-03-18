Their tutorial tutorials: https://rustwasm.github.io/docs/wasm-pack/tutorials/index.html

template-docs: https://rustwasm.github.io/docs/wasm-pack/tutorials/npm-browser-packages/index.html

This site is where my tutorial is from: https://rustwasm.github.io/docs/book/game-of-life/hello-world.html

```wasm-pack build``` to rebuild rust; from ./

```$env:NODE_OPTIONS = "--openssl-legacy-provider"``` was used to say that vulnerabilities don't matter - I wish to be hacked (from https://stackoverflow.com/questions/69692842/error-message-error0308010cdigital-envelope-routinesunsupported)

the site is open at: http://localhost:8080/

```npm install --global serve``` a thing i had to run, not sure if required

```npm run start``` to host that site 2 lines above; form ./www

how to use export/import: https://rustwasm.github.io/wasm-bindgen/reference/attributes/index.html