Snails API
==========

Generates a Swagger spec from a Servant API.

## Viewing the spec

Use [Swagger Editor](https://editor.swagger.io?url=https://raw.githubusercontent.com/ldgrp/snails-api/master/swagger.json) to view `swagger.json`.

## Development

If you do not have `cabal`, I highly recommend [`ghcup`](https://www.haskell.org/ghcup/) or 
reading [this manual](https://www.haskell.org/cabal/).

1. Clone this repo `git clone https://github.com/ldgrp/snails-api`
2. Edit the files in `src/`.
3. Run the main executable with `cabal run` in the main directory.
   This will generate a `swagger.json`.

## Todo
- [ ] servant-client
- [ ] swagger examples
- [ ] servant-docs for markdown a markdown API spec
