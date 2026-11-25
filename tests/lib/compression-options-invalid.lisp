;; Rejected options must not replace the already chosen library options.
(assert (handler-case (progn (js:library-options :compression :xz) nil)
          (error (e) (search "COMPRESSION requires EMBED" (princ-to-string e)))))
(assert (handler-case (progn (js:library-options :embed t :compression :unknown) nil)
          (error (e) (search "COMPRESSION is NIL, :GZIP, :XZ or :ZSTD" (princ-to-string e)))))
