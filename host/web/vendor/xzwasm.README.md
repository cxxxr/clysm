# xzwasm

`xzwasm.mjs` contains the unmodified decoder Wasm bytes and `XzContext`
class extracted from npm `xzwasm@0.1.2`, with ES module exports added.
The upstream ReadableStream wrapper is omitted: Clysm instantiates the
embedded decoder synchronously, without fetch, eval or JSPI.

Upstream: https://github.com/SteveSanderson/xzwasm
Source commit: `87dafdb53d2122baf33720f08f6cfd5f00e216f7`.
Its `Makefile` builds the decoder with wasi-sdk, XZ Embedded
`6f0e0c41e3682254c2e0be245f275f77df821ffe` (public domain) and walloc
`a93409f5ebd49c875514c5fee30d3b151f7b0882` (MIT).
The decoder has no imports. All licenses are in `xzwasm.LICENSE`.

SHA-256 of the npm tarball: `f6bc6836e29a131fbbc6c79fe08117943178c057aba8f38767647807cb47eca3`.
SHA-256 of the decoded Wasm: `bc6e1f81bbcad966562bbdd8fb0849a16e7db104ce68a3053bf81a1c0870146a`.

Only `:compression :xz` libraries copy these files. Generating those
libraries requires the external `xz` command; applications need no external
file or command. The decoder keeps function names in the *application*
module intact, just as the gzip loader does.
