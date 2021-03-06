#! /usr/local/bin/sbcl --script
; ABOVE: for running on sbcl
;#! /home/webapp/acl10.1express/alisp -#!
; ABOVE: for running in AWS.
;#! /p/lisp/acl/linux/latest/alisp -#!
; ABOVE: for running in cs.rochester.edu
;; AUTHOR: Gene Kim  <gkim21@cs.rochester.edu>
;; Works for Allegro Lisp, other Lisps may need a different first #! sequence
;; Should eventually make this portable across Lisp implementations.
;; This script takes a single argument string of the formula to be checked.

;; For some reason the AWS lisp isn't loading Quicklisp automatically in this
;; script.
(load "/home/webapp/quicklisp/setup")
;(load "~/quicklisp/setup")
;(ql:quickload :util :silent t)
;(ql:quickload :ulf-lib :silent t)

(ql:quickload :cl-util :silent t)

(when (not (>= (length (util:argv)) 1))
  (format t "USAGE: sanity-check.cl formula~%")
  (format t "    o  formula in quotes~%")
  (exit))

(load "load")
(in-package :ulf-sanity-checker)

(let* ((use-stdin nil)
       (fh (if use-stdin *standard-output*
                     (open (nth 1 (util:argv))))))
  
  (when fh
    (loop for ulf = (read fh nil)
          while ulf do (format t "~s~%~%####################################~%~%" (sanity-check ulf)))
    (close fh)))

