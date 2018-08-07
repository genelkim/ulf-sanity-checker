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
; - n+preds takes a nou + at least 1 pred
; - Check sentence-level punctuation is at its own unary scope.
; - poss-by takes a single term arg ('s also but post-fixed)
;
; Syntax building reminders
; - Object quotes are acting as terms
; - relative clauses are tensed
;
; Dynamic checks
; - every sentence has a tensed verb
; - (*.p ..) is a predicate
;   -> if (p1.v/a (*.p ..)), then ask if this is an adverb.
;   Exception: (*.p-arg ..) is an argument.
; - each embedded sentence only has one tense op
; - Check plurals with plural form dictionary
; - Record syntax ($) all have at least 1 non-null arg, last arg is not null
;   date-time <= 6 args
;       12 >= month >= 1
;       31 >= day >= 1
;       23 >= hour >= 0
;       59 >= minute >= 0
;       59 >= second >= 0
;   currency <= 2 args
;       currency name : symbol 
;       amount : number
;   address <= 6 args
;       all terms (numbers, names, or (k *.n))
; - check tense (get past-participle list and check)
; 
; Preprocessing checks
; - Check correct punctuation are escaped
; - number of quotes match between surface sentence and ulf
; - try to align words between ULF and sentence (ask if omitted/extra ones are necessary).

;; Returns a list of lst with cndn filter out followed by lst with only cndn.
(defun split-by-cond (lst cndn)
  (list (remove-if cndn lst)
        (remove-if-not cndn lst)))

;; Condition to check if an element is a filitered sentence-level operator.
;; Basically all sentence-level operators that are written as phrasal in the
;; surface form.
(defun phrasal-sent-op? (e)
  (or
    (adv-e? e)
    (adv-s? e)
    (adv-f? e)
    (member e '(not not.adv-e not.adv-s))))


;; Merge object quotes.
;; Filter commas, semicolons, integrated quote marks.
;; Extract sentence-level operators that are phrasal in surface form:
;;  not, adv-e, adv-s, adv-f
;; Replace aliases?
;; Returns (preprocessed-f, sent-ops)
(defun preprocess (f)
  (labels
    (
          
     ; Extract sentence-level operators.
     ; Returns (sent-op-filtered-f, list-of-sent-ops)
     (extract-sent-ops
       (f)
       (if (atom f) (list f '())
         (let* ((split (split-by-cond f #'phrasal-sent-op?))
                (no-sent-ops (first split))
                (sent-ops (second split))
                (recursed (mapcar #'extract-sent-ops no-sent-ops)))
           (list (mapcar #'first recursed)
                 (apply #'append (cons sent-ops (mapcar #'second recursed)))))))
     (remove-extra-parens
       (f)
       (cond
         ((atom f) f)
         ((= (length f) 1) (remove-extra-parens (car f)))
         (t (mapcar #'remove-extra-parens f))))
     ); end of labels definitions.
    (let ((pair (extract-sent-ops f)))
      (list (remove-extra-parens (first pair)) (second pair)))))
        



;; Recursively check for bad patterns.
;; The types are guessed through TTT patterns.
;; Returns a list of pairs with user display information.
;; 1. pattern that failed a test.
;; 2. a list of messages about conditions for failed phenomena.
(defun bad-pattern-check (f)
  (labels 
    ((single-bad-pattern-eval
       (segment pair)
       (if (apply (first pair) (list segment))
         (list segment (list (second pair)))
         nil))
     (bad-pattern-eval
       (x)
       (let ((indres (mapcar #'(lambda (pair) 
                                 (single-bad-pattern-eval x pair)) 
                             *bad-pattern-test-pairs*)))
         (if (apply #'append indres)
           ;; Merge the messages into a list of messages.
           (list (caar (remove-if #'null indres)) 
                 (apply #'append 
                        (mapcar #'second indres)))
           nil)))); end of labels definitions.
    (cond
      ((atom f) '())
      (t (let ((recres (apply #'append (mapcar #'bad-pattern-check f)))
               (curres (bad-pattern-eval f)))
           (if curres
             (cons curres recres)
             recres))))))






(defun sanity-check (f)
  (let* ((preprocd (preprocess f))
         (patternres 
           (apply #'append
                      (mapcar #'bad-pattern-check (cons (first preprocd)
                                                        (second preprocd))))))
    (format t "Sanity checking formula (after preprocessing).~%")
    (format t "~s~%" preprocd)
    (format t "Formula with predicted types.~%")
    (format t "~s~%~%" (mapcar #'label-formula-types preprocd))
    
    (if patternres
      ;; Found some patterns.
      (dolist (x patternres)
        (let ((segment (first x))
              (msgs (second x)))
          (format t "Ann segment: ~s~%" segment)
          (format t "Predicted constituent types ((list of types) -- constituent)~%") 
          (dolist (arg segment) 
            (format t "  ~s~%" (list (ulf-type? arg) '-- arg)))
          (format t "Possibly failed conditions:~%")
          (dolist (msg msgs)
            (format t "  ~s~%" msg))
          (format t "~%")))
      ;; No patterns.
      (format t "****No errors detected.****~%"))))



