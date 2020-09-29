# 🐌 Snails (Test) Server & API

~~Generates a Swagger spec from a Servant API.~~

This program will

- Start a [warp][warp] server on port 8081
- Initialize a SQLite database file at `./db.sqlite`
- Populate the SQLite database (see Main.hs)
- Serve the Snails REST API
- Generate Swagger docs

## ⚡ Viewing the spec

Use [Swagger Editor][swagger-editor] to view `swagger.json`.


## 🚀 Usage

This project was built with [Servant][servant] + [Persistent][persistent].

If you do not have `cabal`, I highly recommend [`ghcup`][ghcup] or 
reading [this manual][cabal].

```bash
# Clone this repo
git clone https://github.com/ldgrp/snails-api

# Navigate to project directory
cd snails-api

# Start the server
cabal run
```


## 💻 Development

1. Clone this repo `git clone https://github.com/ldgrp/snails-api`
2. Edit the files in `src/`.
3. Run the main executable with `cabal run` in the main directory.
   This will generate a `swagger.json`.

## 🗒️ Todo

- [ ] Authentication
- [ ] Handle JWT tokens. For protected endpoints, the server will currently accept
      the user id as a "token".
- [x] User API
- [x] Entry API
- [x] Messaging API
- [x] (Mock) Map API
- [x] (Mock) News API
- [x] (Mock) Transport API
- [x] (Mock) Weather API
- [ ] Use esqueleto. Persistent does _not_ support SQL joins 😞
- [ ] Postgres support
- [ ] Tests

[persistent]:https://www.yesodweb.com/book/persistent
[servant]: https://www.servant.dev/
[swagger-editor]: https://editor.swagger.io?url=https://raw.githubusercontent.com/ldgrp/snails-api/master/swagger.json
[ghcup]: https://www.haskell.org/ghcup/
[cabal]: https://www.haskell.org/cabal/
[warp]: https://hackage.haskell.org/package/warp