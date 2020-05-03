;; Gene Louis Kim, 5-2-2020
;; Unit tests for files I had before.

(in-package :ulf-sanity-checker)

(setq *print-failures* t)
(setq *print-errors* t)
(setq *print-summary* t)
(setq *summarize-results* t)

(define-test no-error-sanity-tests
  "Ensure these ULFs don't raise errors"
  (:tag :initial :no-error)
  (let ((ulfs
         '((((three.d four.d or.cc six.d) (plur person.n)) (past leave.v))
           )))
    (loop for ulf in ulfs
          do (assert-equal nil (sanity-check ulf :silent? t)))))

