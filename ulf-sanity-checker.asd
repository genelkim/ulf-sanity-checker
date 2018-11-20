;; ULF Sanity Checker
;; Packaged on 2018-11-15

(asdf:defsystem :ulf-sanity-checker
  :name "ulf-sanity-checker"
  :version "0.3.0"
  :author "Gene Louis Kim"
  :depends-on (:ttt :util)
  :components ((:file "package")
               (:file "ttt-lexical-patterns")
               (:file "ttt-phrase-patterns")
               (:file "ttt-bad-patterns")
               (:file "macros")
               (:file "ulf-sanity-checker"))) 

