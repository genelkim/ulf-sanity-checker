;; ULF Sanity Checker
;; Packaged on 2018-11-15

(in-package :cl-user)

(defpackage :ulf-sanity-checker
  (:use :cl :ttt :util :ulf-lib)
  (:export sanity-check))

;; Inherit all the ulf-lib external symbols.
;; ulf-lib contains ULF segment matching functions (e.g. noun?) which are used
;; in the TTT patterns.
(use-package :ulf-lib)

