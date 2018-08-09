#! /home/ec2-user/acl10.1express/alisp -#!
;; AUTHOR: Gene Kim  <gkim21@cs.rochester.edu>
;; Works for Allegro Lisp, other Lisps may need a different first #! sequence
;; Should eventually make this portable across Lisp implementations.

(when (not (>= (length (sys:command-line-arguments)) 1))
  (format t "USAGE: sanity-check formula~%")
  (format t "   o  formula in quotes ~%")
  (exit))

(load "init")

(let* ((use-stdin nil)
       (formulahandle (if use-stdin *standard-output*
                     (open (nth 1 (sys:command-line-arguments)))))
       (formula (read formulahandle)))
  (format t "~s~%" (sanity-check formula)))

