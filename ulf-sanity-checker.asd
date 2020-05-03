;; ULF Sanity Checker
;; Packaged on 2018-11-15

(asdf:defsystem :ulf-sanity-checker
  :name "ulf-sanity-checker"
  :version "0.3.0"
  :author "Gene Louis Kim"
  :depends-on (:ttt :cl-util :ulf-lib :alexandria :lisp-unit)
  :components ((:file "package")
               (:file "ttt-bad-patterns")
               (:file "ulf-sanity-checker")
               (:file "ulf-ctp-interface")))

