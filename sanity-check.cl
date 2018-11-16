#! /home/ec2-user/acl10.1express/alisp -#!
; ABOVE: for running in AWS.
;#! /p/lisp/acl/linux/latest/alisp -#!
; ABOVE: for running in cs.rochester.edu
;; AUTHOR: Gene Kim  <gkim21@cs.rochester.edu>
;; Works for Allegro Lisp, other Lisps may need a different first #! sequence
;; Should eventually make this portable across Lisp implementations.
;; This script takes a single argument string of the formula to be checked.

(when (not (>= (length (sys:command-line-arguments)) 1))
  (format t "USAGE: sanity-check.cl formula~%")
  (format t "    o  formula in quotes~%")
  (exit))

(load "init")

(let* ((use-stdin nil)
       (fh (if use-stdin *standard-output*
                     (open (nth 1 (sys:command-line-arguments))))))
  
  (when fh
    (loop for ulf = (read fh nil)
          while ulf do (format t "~s~%~%####################################~%~%" (sanity-check ulf)))
    (close fh)))

