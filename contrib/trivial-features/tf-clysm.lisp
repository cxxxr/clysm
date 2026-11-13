;;;; tf-clysm.lisp — trivial-features for Clysm (WebAssembly is little-endian)
(pushnew :little-endian *features*)
(pushnew :unix *features*)
(pushnew :linux *features*)
