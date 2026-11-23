(js:library-options :lazy t)
(defvar *starts* 0)
(js:export "status"
           (lambda ()
             (js:object "starts" *starts* "worker" (js:entry-context "worker")
                        "cli" (js:entry-context "cli")))
           :entry "api")
;; These must not shadow the generated initialization helper's host globals.
(js:export "process" 42 :entry "cli")
(js:export "Error" 43 :entry "cli")
(js:export "globalThis" 44 :entry "cli")
(js:entrypoint (lambda (context)
                (incf *starts*)
                (js:set (js:global) "__clysm_entry_worker" context))
              :entry "worker"
              :when '(:and (:equal (:global "process" "env" "CLYSM_ENTRY_FLAG") "1")
                           (:function (:global "__clysm_entry_gate" "ready"))
                           (:not (:present (:global "__clysm_entry_missing")))))
(js:entrypoint (lambda (context)
                (incf *starts*)
                (js:set (js:global) "__clysm_entry_cli"
                        (js:object "context" context
                                   "basename" (js:call (js:module "node:path") "basename" "/a/b")
                                   "platform" (js:call (js:module "node:os") "platform"))))
              :entry "cli" :imports '(("node:path" "default") ("node:os" "*"))
              :environment '("CLYSM_ENTRY_VALUE" "CLYSM_ENTRY_UNSET") :shebang t :errors :stderr)
(js:entrypoint (lambda (context)
                (declare (ignore context))
                (js:throw (js:get-raw (js:global) "__clysm_entry_sentinel") :message "initialization failed"))
              :entry "throwing")

(js:entrypoint (lambda (context)
                (incf *starts*)
                (js:set (js:global) "__clysm_entry_checked" context))
              :entry "checked" :errors :stderr
              :when '(:not (:equal (:global "__clysm_skip_checked") t))
              :checks '(((:and (:function (:global "__clysm_engine" "suspending"))
                               (:function (:global "__clysm_engine" "promising")))
                         (:concat "need both functions (version "
                                  (:global "__clysm_engine" "version") ")")))
              :load-errors '(("ENOENT" "the application Wasm file is missing")))
(js:entrypoint (lambda (context)
                (declare (ignore context))
                (js:throw (js:get-raw (js:global) "__clysm_entry_sentinel") :message "initializer failed"))
              :entry "checked-throwing"
              :load-errors '(("ENOENT" "must not replace an initializer error")))
