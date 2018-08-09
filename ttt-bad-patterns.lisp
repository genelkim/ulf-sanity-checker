;; TTT patterns for errors.

(defparameter *ttt-bad-det*
  '(!1 (det? _! _+)
      (det? (! ~ noun?))
      (_+ det? _*)))
(defparameter *bad-det-msg* 
  "Determiners take 1 nominal (noun) argument.")

(defparameter *ttt-bad-prep*
  '(!1 (lex-p? _! _+)
      (lex-p? (! ~ term?))
      (_+ lex-p? _*)))
(defparameter *bad-prep-msg* 
  "Simple prepositions (*.p) take terms as arguments and take 1 argument.")

(defparameter *ttt-bad-be*
  '(!1 (be.v _! _+)
      (be.v (! ~ pred?))
      ((lex-tense? be.v) _! _+)
      ((lex-tense? be.v) (! ~ pred?))))
(defparameter *bad-be-msg* 
  "be.v takes a single predicate argument.")

(defparameter *ttt-bad-tensed-sent-op*
  '(!1 (tensed-sent-op? _! _+)
      (tensed-sent-op? (! ~ tensed-sent?))))
(defparameter *bad-tensed-sent-op-msg* 
  "that/tht/*.ps take a single tensed sentence argument.")

(defparameter *ttt-bad-untensed-sent-op*
  '(!1 (untensed-sent-op? _! _+)
      (untensed-sent-op? (! ~ sent?))
      (_+ untensed-sent-op? _*)))
(defparameter *bad-untensed-sent-op-msg* 
  "ke takes a single untensed sentence argument.")


(defparameter *ttt-bad-action-reifier*
  '(!1 (action-reifier? _! _+)
      (action-reifier? (! ~ verb?))
      (_+ action-reifier? _*)))
(defparameter *bad-action-reifier-msg* 
  "ka/to take a single untensed verb argument.")

(defparameter *ttt-bad-plur*
  '(!1 (plur _! _+)
      (plur (! ~ noun?))
      (_+ plur _*)))
(defparameter *bad-plur-msg* 
  "plur takes a single nominal (noun) argument.")

(defparameter *ttt-bad-aux*
  '(!1 (aux? _! _+)
      (aux? (! ~ verb?))
      (tensed-aux? _! _+)
      (tensed-aux? (! ~ verb?))
      (_+ tensed-aux? _*)))
(defparameter *bad-aux-msg* 
  "auxiliaries take a single untensed verb argument.")

(defparameter *ttt-bad-advformer*
  '(!1 (advformer? _! _+)
      (advformer? (! ~ pred?))
      (_+ advformer? _*)))
(defparameter *bad-advformer-msg* 
  "Adverb formers (adv-a, adv-e,..) take a single predicate argument and cannot be used as an argument.")

(defparameter *ttt-bad-detformer*
  '(!1 (detformer? _! _+)
      (detformer? (! ~ adj?))))
(defparameter *bad-detformer-msg*  
  "Determiner formers (nquan, fquan) take a single adjective argument.")

(defparameter *ttt-bad-np-preds*
  '(!1 (_+ np+preds _*) ; np+preds not used as prefix operator.
      (np+preds _!) ; Only 1 arg.
      (np+preds (! ~ term?) _+) ; first argument is not a term.
      (np+preds term? _*.1 (! ~ pred?) _*.2))) ; argument that is not the first is not a predicate.
      
(defparameter *bad-np-preds-msg* 
  "np+preds takes at least 2 arguments where the first is a term and the rest are predicates.")


(defparameter *ttt-bad-n-preds*
  '(!1 (_+ n+preds _*) ; n+preds not used as prefix operator.
      (n+preds _!) ; Only 1 arg.
      (n+preds (! ~ noun?) _+) ; first argument is not a term.
      (n+preds noun? _*1 (! ~ pred?) _*2) ; argument that is not the first is not a predicate.
      ))
(defparameter *bad-n-preds-msg* 
  "n+preds takes at least 2 arguments where the first is a noun and the rest are predicates.")

(defparameter *ttt-bad-sent-punct*
  '(!1 (_! _+ sent-punct?) ; more than 1 arg.
      ((! ~ tensed-sent?) sent-punct?))) ; arg is not a tensed sentence.
(defparameter *bad-sent-punct-msg* 
  "Sentence punctuation take a single tensed sentence argument and is post-fixed.")

(defparameter *ttt-bad-double-tense*
  '(!1 (tensed-aux? tensed-verb?)
       (tensed-verb? tensed-verb?)))
(defparameter *bad-double-tense-msg*
  "Each embedded sentence should only have 1 tense operator.")


(defparameter *ttt-no-periods-or-commas*
  '(! \, \.))
(defparameter *no-periods-or-commas-msg*
  "Annotating commas and periods is no longer supported.")

;; Function definitions for this.
(defun bad-det? (x) (ttt:match-expr *ttt-bad-det* x))
(defun bad-prep? (x) (ttt:match-expr *ttt-bad-prep* x))
(defun bad-be? (x) (ttt:match-expr *ttt-bad-be* x))
(defun bad-tensed-sent-op? (x) (ttt:match-expr *ttt-bad-tensed-sent-op* x))
(defun bad-untensed-sent-op? (x) (ttt:match-expr *ttt-bad-untensed-sent-op* x))
(defun bad-action-reifier? (x) (ttt:match-expr *ttt-bad-action-reifier* x))
(defun bad-plur? (x) (ttt:match-expr *ttt-bad-plur* x))
(defun bad-aux? (x) (ttt:match-expr *ttt-bad-aux* x))
(defun bad-advformer? (x) (ttt:match-expr *ttt-bad-advformer* x))
(defun bad-detformer? (x) (ttt:match-expr *ttt-bad-detformer* x))
(defun bad-np-preds? (x) (ttt:match-expr *ttt-bad-np-preds* x))
(defun bad-n-preds? (x) (ttt:match-expr *ttt-bad-n-preds* x))
(defun bad-sent-punct? (x) (ttt:match-expr *ttt-bad-sent-punct* x))
(defun bad-double-tense? (x) (ttt:match-expr *ttt-bad-double-tense* x))
(defun no-periods-or-commas? (x) (ttt:match-expr *ttt-no-periods-or-commas* x))

(defparameter *bad-pattern-test-pairs*
  (list
    (list #'bad-det? *bad-det-msg*)
    (list #'bad-prep? *bad-prep-msg*)
    (list #'bad-be? *bad-be-msg*)
    (list #'bad-tensed-sent-op? *bad-tensed-sent-op-msg*)
    (list #'bad-untensed-sent-op? *bad-untensed-sent-op-msg*)
    (list #'bad-action-reifier? *bad-action-reifier-msg*)
    (list #'bad-plur? *bad-plur-msg*)
    (list #'bad-aux? *bad-aux-msg*)
    (list #'bad-advformer? *bad-advformer-msg*)
    (list #'bad-detformer? *bad-detformer-msg*)
    (list #'bad-np-preds? *bad-np-preds-msg*)
    (list #'bad-n-preds? *bad-n-preds-msg*)
    (list #'bad-sent-punct? *bad-sent-punct-msg*)
    (list #'bad-double-tense? *bad-double-tense-msg*)
    (list #'no-periods-or-commas? *no-periods-or-commas-msg*)))

