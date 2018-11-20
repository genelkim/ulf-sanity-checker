;; Sanity checker for ULF annotations.
;; This checks for possible errors using simple pattern matching methods, 
;; recursing as much as it can.  This is NOT a full-fledged syntax checker.

; Checks:
; Bad Patterns
; - determiners only take noun arguments
; - determiners only take 1 argument
; - prepositions only take terms
; - prepositions only take 1 argument
; - be.v only takes a predicate
; - be.v only takes two arguments.
; - tht/that takes a single tensed sentence.
; - ps takes a single tensed sentence
; - ke takes a single untensed sentence.
; - ka/to takes an *untensed* verb
; - plur only takes a single noun
; - perf and auxiliaries take a single untensed verb arg.
; - adv-* only take 1 predicate arg
; - fquan/nquan take adjective predicates 
; - np+preds takes a term + at least 1 pred
; - n+preds takes a noun + at least 1 pred
; - Check sentence-level punctuation is at its own unary scope.
; - poss-by takes a single term arg ('s also but post-fixed)
; - there are no periods or commas (they're depricated)
; - *.ps annotation doesn't have and adv forming wrapper
; - possessives are ((<term> 's) <noun>)
; - pu takes a single argument
; - predicate modifications should be done in operator-operand pairs (non-flat)
; - brackets should not scope around a single member
;
; Syntax building reminders
; - Object quotes are acting as terms
; - relative clauses are tensed
;
; Dynamic checks (TODO)
; - every sentence has a tensed verb
; - (*.p ..) is a predicate
;   -> if (p1.v/a (*.p ..)), then ask if this is an adverb.
;   Exception: (*.p-arg ..) is an argument.
; - each embedded sentence only has one tense op
; - check tense (get past-participle list and check)
; 
; Preprocessing checks (TODO)
; - try to align words between ULF and sentence (ask if omitted/extra ones are necessary).
; - run sub/rep since they are type-general (too much work to add into the type system).

(in-package :ulf-sanity-checker)

;; Condition to check if an element is a filitered sentence-level operator.
;; Basically all sentence-level operators that are written as phrasal in the
;; surface form.
(defun phrasal-sent-op? (e)
  (or
    (adv-e? e)
    (adv-s? e)
    (adv-f? e)
    (member e '(not not.adv-e not.adv-s))))

(defun hide-ttt-ops (wff); tested
;~~~~~~~~~~~~~~~~~~~~~~~~
; Wrap [..] around symbols like !, +, ?, *, @, ~, {}, or <>, or
; ones starting this way, which we may want to use in some patterns
; (e.g., in wff-patterns involving *, **, @, or ~), but can't 
; because of their special meanings in TTT. We're assuming that
; the wffs we want to process don't *already* contain symbols in
; square brackets, starting as above inside the brackets, and which
; shouldn't have the brackets removed when we ultimately "unhide"
; the hidden symbols in a formula.
;
  (let (str chars)
       (cond ((symbolp wff)
              (setq str (string wff))
              (setq chars (coerce str 'list))
              (cond ((member (car chars) '(#\! #\+ #\? #\* #\@ #\~))
                     (intern (concatenate 'string "[" str "]")))
                    ((and (eq (car chars) #\{) (eq (second chars) #\}))
                     (intern (concatenate 'string "[" str "]")))
                    ((and (eq (car chars) #\<) (eq (second chars) #\>))
                     (intern (concatenate 'string "[" str "]")))
                    (t wff)))
             ((atom wff) wff)
             (t (cons (hide-ttt-ops (car wff)) (hide-ttt-ops (cdr wff)))))
 )); end of hide-ttt-ops


(defun unhide-ttt-ops (wff); tested
;~~~~~~~~~~~~~~~~~~~~~~~~~~
; Remove the square brackets that have been added around ttt symbols
; in wff by 'hide-ttt-ops':
;
 (let (str chars)
      (cond ((symbolp wff)
             (setq str (string wff))
             (setq chars (coerce str 'list))
             (cond ((or (not (eq (car chars) #\[))
                        (not (eq (car (last chars)) #\]))) wff)
                   (t (setq chars (cdr (butlast chars)))
                      (setq str (coerce chars 'string))
                      (cond ((null chars) wff)
                            ((member (car chars) '(#\! #\+ #\? #\* #\@ #\~))
                             (intern str))
                            ((and (eq (car chars) #\{) (eq (second chars) #\}))
                             (intern str))
                            ((and (eq (car chars) #\<) (eq (second chars) #\>))
                             (intern str))
                            (t wff)))))
            ((atom wff) wff)
            (t (cons (unhide-ttt-ops (car wff)) (unhide-ttt-ops (cdr wff)))))
 )); end of unhide-ttt-ops


;; Walks through the formula f and extracts out categories that satisfy catfn.
;; Formula constituents that satisfy ign-cnd-fn are ignored, so elements 
;; satisfying catfn directly within them are ignored.
(defun extract-category (f catfn ign-cnd-fn)
  (if (atom f) (list f '())
    (let* ((split 
             ;; Only filter out sentence ops if there are at least two
             ;; elements.  If there are two, then we treat sentence 
             ;; operators as locally applied.
             (if (funcall ign-cnd-fn f)
               (list f nil)
               (util:split-by-cond f catfn)))
           (no-sent-ops (first split))
           (sent-ops (second split))
           (recursed (mapcar #'(lambda (x) 
                                 (extract-category x catfn ign-cnd-fn)) 
                             no-sent-ops)))
      (list (mapcar #'first recursed)
            (apply #'append (cons sent-ops (mapcar #'second recursed)))))))

;; Extract sentence-level operators that are phrasal in surface form:
;;  not, adv-e, adv-s, adv-f
;; Apply sub macros
;; Uninvert verb/auxiliary inversions for questions.
;; Replace aliases?
;; Returns (preprocessed-f, sent-ops)
(defun preprocess (f)
  (labels
    (
     ; Extract sentence-level operators.
     ; Returns (sent-op-filtered-f, list-of-sent-ops)
     (extract-sent-ops
       (f)
       (extract-category f #'phrasal-sent-op? 
                         #'(lambda (x) (<= (length x) 2))))

     ;; Extracts vocatives.
     (extract-vocs (f)
       (extract-category f #'voc? #'(lambda (x) (declare (ignore x)) nil)))

     ;; Removes double parens.
     (remove-extra-parens
       (f)
       (cond
         ((atom f) f)
         ((= (length f) 1) (remove-extra-parens (car f)))
         (t (mapcar #'remove-extra-parens f))))
     ); end of labels definitions.
    
    ;; Main body, run the preprocessing functions.
    (let* ((subres (multiple-value-list
                     (apply-sub-macro f)))
           (subf (second subres))
           (adv-a-lifted (lift-adv-a subf))
           (sent-op-pair (extract-sent-ops adv-a-lifted))
           (voc-pair (extract-vocs sent-op-pair))
           (main-sent (first (first voc-pair)))
           (sent-ops (second (first voc-pair)))
           (vocs (second voc-pair))
           (paren-remvd-main-sent (remove-extra-parens main-sent))
           (uninv (uninvert-verbauxes paren-remvd-main-sent))
           (regrouped (list uninv sent-ops vocs)))
      ;(format t "sent-op-pair ~s~%~%" sent-op-pair)
      ;(format t "voc-pair ~s~%~%" voc-pair)
      ;(format t "main-sent ~s~%~%" main-sent)
      ;(format t "paren-remvd-main-sent ~s~%~%" paren-remvd-main-sent)
      ;(format t "uninv ~s~%~%" uninv)
      regrouped)))


;; Recursively check for bad patterns on formula f.
;; pattern-test-pairs is a list of pairs of functions and corresponding
;; messages.  The functions are evaluated and if t, the message is returned.
;; Returns a list of pairs with user display information.
;; 1. pattern that failed a test.
;; 2. a list of messages about conditions for failed phenomena.
(defun bad-pattern-check (f pattern-test-pairs)
  (labels 
    (
     ;; Evaluates a formula fragment ons a simple test/msg pair.
     (single-bad-pattern-eval (segment pair)
       (if (apply (first pair) (list segment))
         (list segment (list (second pair)))
         nil))
     ;; Evaluates segment 'x' on all 'pattern-test-pairs'.
     (bad-pattern-eval (x)
       (let ((indres (mapcar #'(lambda (pair) 
                                 (single-bad-pattern-eval x pair)) 
                             pattern-test-pairs)))
         (if (apply #'append indres)
           ;; Merge the messages into a list of messages.
           (list (caar (remove-if #'null indres)) 
                 (apply #'append 
                        (mapcar #'second indres)))
           nil)))); end of labels definitions.
    ;; Main body.
    (cond
      ((atom f) '())
      (t (let ((recres (apply #'append 
                              (mapcar #'(lambda (x) 
                                          (bad-pattern-check x pattern-test-pairs))
                                      f)))
               (curres (bad-pattern-eval f)))
           (if curres
             (cons curres recres)
             recres))))))


;; Main sanity checking function.
(defun sanity-check (f)
  (let* ((rawpatternres
             (bad-pattern-check 
               (hide-ttt-ops f) 
               *raw-bad-pattern-test-pairs*))
         (preprocd (preprocess f))
         (linesep (format nil "************************************~%"))
         (patternres 
           (apply #'append
                  (mapcar #'(lambda (x)
                              (bad-pattern-check x *bad-pattern-test-pairs*))
                          preprocd)))
          allres)

    (format t linesep)
    (format t "Sanity checking formula (before preprocessing).~%")
    (format t linesep)
    (format t "~s~%~%" f)
    
    (format t linesep)
    (format t "Sanity checking formula (after preprocessing).~%")
    (format t linesep)
    (format t "~s~%~%" preprocd)
    
    (format t linesep)
    (format t "Possible errors~%")
    (format t linesep)
    
    (setq allres (append rawpatternres patternres))
    (if allres
      ;; Found some patterns.
      (dolist (x allres)
        (let ((segment (first x))
              (msgs (second x)))
          (format t "~%Possibly failed conditions:~%")
          (dolist (msg msgs)
            (format t "  ~s~%" msg))
          (format t "Ann segment:~%  ~s~%" segment)
          (format t "Predicted constituent types ((list of types) -- constituent)~%") 
          (dolist (arg segment) 
            (format t "  ~s~%" (list (ulf-type? arg) '-- arg)))))
      ;; No patterns.
      (format t "****No errors detected.****~%"))
    
    (format t "~%")
    (format t linesep) 
    (format t "Formula with predicted types (for debugging).~%")
    (format t linesep)
    (format t "~s~%~%" (mapcar #'label-formula-types preprocd))
    ))
