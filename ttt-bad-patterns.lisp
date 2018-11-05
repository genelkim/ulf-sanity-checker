;; TTT patterns for errors.

(defparameter *ttt-bad-det*
  '(!1 (det? _! _+)
      ;; The only things that can be arguments to determiners are nouns,
      ;; rarely prepositions, and typeless predicates (e.g. (= ..)).
      (det? (! ~ noun? pp? (= _!)))
      (_+ det? _*)))
(defparameter *bad-det-msg* 
  "Determiners take 1 nominal (noun) or, rarely, prepositional argument.")

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
  "be.v takes a single predicate argument (unless used after an it-cleft -- ignore this if that's the case).")

(defparameter *ttt-bad-tensed-sent-op*
  '(!1 ((!2 tensed-sent-reifier? lex-ps?) _! _+)
      ((!3 tensed-sent-reifier? lex-ps?) (! ~ tensed-sent?))))
(defparameter *bad-tensed-sent-op-msg* 
  "that/tht/whether/ans-to take a single tensed sentence argument.")

(defparameter *ttt-bad-sent-op*
  '(!1 (sent-reifier? _! _+)
      (sent-reifier? (! ~ sent?))
      (_+ sent-reifier? _*)))
(defparameter *bad-sent-op-msg* 
  "ke takes a single untensed sentence argument.")

(defparameter *ttt-bad-verb-reifier*
  '(!1 (verb-reifier? _! _+)
      (verb-reifier? (! ~ verb?))
      (_+ verb-reifier? _*)))
(defparameter *bad-verb-reifier-msg* 
  "ka/to/gd take a single untensed verb argument.")

(defparameter *ttt-bad-noun-reifier*
  '(!1 (noun-reifier? _! _+)
       (noun-reifier (! ~ noun?))
       (_+ noun-reifier? _*)))
(defparameter *bad-noun-reifier-msg* 
  "k takes a single nominal argument.")

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
      (n+preds (! ~ noun?) _+) ; first argument is not a noun.
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

(defparameter *ttt-old-ps-ann*
  '((! adv-e adv-a adv-s) 
    (lex-ps? tensed-sent?)))
(defparameter *old-ps-ann-msg* 
  "(adv-s (*.ps ...)) is no longer the way to annotate *.ps.")

(defparameter *ttt-bad-possessive*
  '(!1 (((!2 ~ term?) 's) noun?) ; first arg is not a term
       ((term? 's) (!2 ~ noun?)) ; second arg is not a noun
       ('s _+)      ; 's used as a prefix operator
       (_+1 's _+2) ; 's used flat
        ))
(defparameter *bad-possessive-msg*
  "The 's operator takes a term (post-fixed), followed by a noun (prefixed), and curried.")

(defparameter *ttt-bad-pu*
  '(pu _! _+))
(defparameter *bad-pu-msg*
  "The 'pu' operator takes a single phrase.")

(defparameter *ttt-bad-flat-mod*
  '(_*1 
     (!2 adj? noun? term?) 
     (!3 adj? noun? term?)
     (!4 adj? noun? term?)
     _*2))
(defparameter *bad-flat-mod-msg*
  "Predicate modifications should be scoped into operator-operand pairs.")

(defparameter *ttt-bad-single-bracket*
  '(_!))
(defparameter *bad-single-bracket-msg*
  "Brackets should not scope around a single constituent (need at least two members in its scope as an operator-operand pair).")

(defun contains-sub-var? (x)
  (ttt:match-expr '(^* [*h]) x))
(defparameter *ttt-bad-sub*
  '(!1 
     (sub _!)
     (sub)
     (sub _!2 _!3 _+)
     (_+ sub _*)
     (sub _!4 (!5 ~ contains-sub-var?))
     ))
(defparameter *bad-sub-msg*
  "'sub' operator should take two arguments and the second argument should contain a '*h'")

;; Function definitions for this.
(defun bad-det? (x) (ttt:match-expr *ttt-bad-det* x))
(defun bad-prep? (x) (ttt:match-expr *ttt-bad-prep* x))
(defun bad-be? (x) (ttt:match-expr *ttt-bad-be* x))
(defun bad-tensed-sent-op? (x) (ttt:match-expr *ttt-bad-tensed-sent-op* x))
(defun bad-sent-op? (x) (ttt:match-expr *ttt-bad-sent-op* x))
(defun bad-verb-reifier? (x) (ttt:match-expr *ttt-bad-verb-reifier* x))
(defun bad-noun-reifier? (x) (ttt:match-expr *ttt-bad-noun-reifier* x))
(defun bad-plur? (x) (ttt:match-expr *ttt-bad-plur* x))
(defun bad-aux? (x) (ttt:match-expr *ttt-bad-aux* x))
(defun bad-advformer? (x) (ttt:match-expr *ttt-bad-advformer* x))
(defun bad-detformer? (x) (ttt:match-expr *ttt-bad-detformer* x))
(defun bad-np-preds? (x) (ttt:match-expr *ttt-bad-np-preds* x))
(defun bad-n-preds? (x) (ttt:match-expr *ttt-bad-n-preds* x))
(defun bad-sent-punct? (x) (ttt:match-expr *ttt-bad-sent-punct* x))
(defun bad-double-tense? (x) (ttt:match-expr *ttt-bad-double-tense* x))
(defun no-periods-or-commas? (x) (ttt:match-expr *ttt-no-periods-or-commas* x))
(defun old-ps-ann? (x) (ttt:match-expr *ttt-old-ps-ann* x))
(defun bad-possessive? (x) (ttt:match-expr *ttt-bad-possessive* x))
(defun bad-pu? (x) (ttt:match-expr *ttt-bad-pu* x))
(defun bad-flat-mod? (x) (ttt:match-expr *ttt-bad-flat-mod* x))
(defun bad-single-bracket? (x) (ttt:match-expr *ttt-bad-single-bracket* x))
(defun bad-sub? (x) (ttt:match-expr *ttt-bad-sub* x))

(defparameter *bad-pattern-test-pairs*
  (list
    (list #'bad-det? *bad-det-msg*)
    (list #'bad-prep? *bad-prep-msg*)
    (list #'bad-be? *bad-be-msg*)
    (list #'bad-tensed-sent-op? *bad-tensed-sent-op-msg*)
    (list #'bad-sent-op? *bad-sent-op-msg*)
    (list #'bad-verb-reifier? *bad-verb-reifier-msg*)
    (list #'bad-noun-reifier? *bad-noun-reifier-msg*)
    (list #'bad-plur? *bad-plur-msg*)
    (list #'bad-aux? *bad-aux-msg*)
    (list #'bad-advformer? *bad-advformer-msg*)
    (list #'bad-detformer? *bad-detformer-msg*)
    (list #'bad-np-preds? *bad-np-preds-msg*)
    (list #'bad-n-preds? *bad-n-preds-msg*)
    (list #'bad-sent-punct? *bad-sent-punct-msg*)
    (list #'bad-double-tense? *bad-double-tense-msg*)
    (list #'no-periods-or-commas? *no-periods-or-commas-msg*)
    (list #'old-ps-ann? *old-ps-ann-msg*)
    (list #'bad-possessive? *bad-possessive-msg*)
    (list #'bad-pu? *bad-pu-msg*)
    (list #'bad-flat-mod? *bad-flat-mod-msg*)
    ))

;; Same as above but run on raw formulas (before preprocessing).
(defparameter *raw-bad-pattern-test-pairs*
  (list
    (list #'bad-single-bracket? *bad-single-bracket-msg*)
    (list #'bad-sub? *bad-sub-msg*)
    ))

