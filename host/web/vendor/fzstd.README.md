# fzstd

`fzstd.mjs` is the unmodified `esm/index.mjs` from npm `fzstd@0.1.1`,
https://github.com/101arrowz/fzstd. The MIT license is copied alongside it
as `fzstd.LICENSE`.

SHA-256 of the npm tarball:
`5ed2ee2d3439067be211d0093292c708a831045372342c897437e5a4997c45c5`

SHA-256 of `fzstd.mjs`:
`1a901fc5a58c349bbdb4f2aa1e688637ef56891ec58243a9f0cd863166b0d2a4`

Only the synchronous `decompress` is used. A generated embedded library
includes these files only when it requests `:compression :zstd`.
Compression itself uses Node's built-in zlib when the library is generated
(Node.js 22.15 or later), so building Clysm does not need npm.
