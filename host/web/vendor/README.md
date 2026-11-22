# fflate

`fflate.mjs` is the unmodified `esm/browser.js` from npm `fflate@0.8.3`,
https://github.com/101arrowz/fflate/releases/tag/v0.8.3.
The MIT license is copied alongside it as `fflate.LICENSE`.

SHA-256 of the npm tarball:
`38c2cd824402407b43153c782274aec2ea83ea688e4aa0b743c5f2c305857d92`

SHA-256 of `fflate.mjs`:
`b7ca4450b19559a1d50eb381adcee94b82449674be4cd17789d9beba7e6122a1`

Only the synchronous gzip decoder is used. A generated embedded library
includes these files only when it requests `:compression :gzip`. Bundlers
can remove the other exports. Compression itself uses Node's built-in zlib
when the library is generated, so building Clysm does not need npm.
