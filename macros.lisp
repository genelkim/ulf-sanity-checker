;;; Gene Kim 8-12-2018
;;;
;;; Macro expansions that are necessary for the sanity checker.



;;; Returns t if 'ulf' contains *h, nil otherwise.
(defun contains-hole (ulf)
  (cond
    ((and (atom ulf) (eq '*h ulf)) t)
    ((atom ulf) nil)
    (t (or (contains-hole (car ulf))
           (contains-hold (cdr ulf))))))

;;; Applies all the sub macros in the given ULF.
;;; Returns a pair of values (success, results)
;;; If fail-on-bad-use is t, this function returns
;;; nil for 'success' and returns the level at which
;;; it failed in 'results'.  
;;;
;;; If 'fail-on-bad-use' is nil, the system will always
;;; return true and naively apply the 'sub' macro.
;;;
;;; Examples of bad use:
;;;   'sub' does not have exactly 2 arguments
;;;   *h is not present in the second argument
(defun apply-sub-macro (ulf &optional fail-on-bad-use)
  (cond
    ((atom ulf) (values t ulf))
    ;; If sub and less than 2 arguments, fail.
    ((and (eq (first ulf) 'sub) (< (length ulf) 3))
     (return-from 'apply-sub-macro (values nil ulf)))
    ;; If sub, recurse into the second arg, then try to apply.
    ((eq (first ulf) 'sub)
     (if (and fail-on-bad-use (not (equal (length ulf) 3)))
       (return-from 'apply-sub-macro (values nil ulf)))
     (multiple-value-bind (recsuc recres) 
                          (apply-sub-macro (third ulf) fail-on-bad-use)
       (cond
         ;; If the recursion failed, propagate results.
         ((not recsuc) (values recsuc recres))
         ;; If the recrusive result doesn't have a *h, return with failure.
         ((and fail-on-bad-use (not (contains-hole recres)))
          (values nil (list (first ulf) (second ulf) recres)))
         ;; Apply substitution and return result.
         (t (values t (subst (second ulf) '*h recres))))))
    ;; Otherwise, just recursive into all.  If there's a failure, return
    ;; it.  Otherwise, merge together.
    (t (let* ((recres (mapcar #'(lambda (x) 
                                  (multiple-value-list 
                                    (apply-sub-macro x fail-on-bad-use)))
                              ulf))
              (successes (mapcar #'first recres))
              (fs (mapcar #'second recres)))
         (if (reduce #'(lambda (x y) (and x y)) successes)
           ;; If all recursion succeeded, return the results with 't'.
           (values t fs)
           ;; Otherwise, find the first one that failed and return it.
           (let ((failpos (position nil successes)))
             (values nil (nth failpos fs))))))))


(defun lex-verbaux? (x)
  (or (lex-verb? x) (aux? x))) 

;;; Rule to uninvert sentences with verb/auxiliary inversion, e.g. questions.
(defparameter *ttt-uninvert-verbaux*
  '(/ ((lex-tense? lex-verbaux?) term? _+)
      (uninvert-verbaux! ((tense? lex-verbaux?) term? _+))))

;;; Takes a sentence of the form 
;;; ((<tense> verb/aux) NP VP ADV1 .. ADVn)
;;; and transforms it to
;;; (NP ((((<tense> verb/aux) VP) ADV1) ... ADVn))
(defun uninvert-verbaux! (ulf)
  (if (< (length ulf) 3)
    (return-from 'uninvert-verbaux! nil))
  (let ((headva (first ulf))
        (np (second ulf))
        (vp (third ulf))
        (remain (cdddr ulf)))
    (list np
          (reduce #'list remain :initial-value (list headva vp)))))



