
(load "ttt-lexical-patterns")

(defparameter *record-types*
  '(date-time currency us_addr))

(defparameter *ttt-noun*
  '(! lex-noun?
      lex-name-pred?
      (adj? noun?)
      (plur noun?)
      (noun? noun?)
      (lex-name? noun?)
      ; (mother-of.n |John|)
      (lex-noun? term?)
      (lex-function? term?)
      (n+preds noun? (+ pred?))
      (noun? lex-coord? (+ noun?))
      
      ;; Fall back if arguments not correctly analyzed.
      (n+preds _+)
      ))

(defparameter *ttt-adj*
  '(! lex-adjective?
      (adj? adj?)
      (adv-a? adj?)
      (poss-by term?)
      ;; Some adjectives take infinitives.
      (adj? (to verb?))
      ;; Some adjectives take arguments.
      (adj? (lex-p-arg? term?))
      (adj? lex-coord? (+ adj?))
      ))

(defparameter *ttt-adv-a*
   '(! lex-adv-a?
      (adv-a pred?)
      ;; Below is not quite correct since some *.pq map to (adv-e ...), but for
      ;; the sake of this syntax checker it doesn't matter.
      lex-pq? 
      (adv-a? lex-coord? (+ adv-a?))
      ))

(defparameter *ttt-adv-e*
   '(! lex-adv-e?
      (adv-e pred?)
      (adv-e? lex-coord? (+ adv-e?))
      ))

(defparameter *ttt-adv-s*
   '(! lex-adv-s?
      (adv-s pred?)
      (adv-s? lex-coord? adv-s?)))

(defparameter *ttt-adv-f*
   '(! lex-adv-f?
      (adv-f pred?)
      (adv-f? lex-coord? adv-f?)
      ))

(defparameter *ttt-pp*
   '(! (lex-p? term?)
       (pp? lex-coord? (+ pp?))))

(defparameter *ttt-term*
  '(! lex-pronoun?
      lex-name?
      lex-number?
      (det? noun?)
      (set-of (+ term?))
      (term? lex-coord? (+ term?))
      ;; Reified
      (noun-reifier? noun?)
      (verb-reifier? verb?)
      (sent-reifier? sent?)
      (tensed-sent-reifier? tensed-sent?)
      ;; Domain specific syntax.
      (ds _! litstring?)
      ;; Prepositional argument.s
      (lex-p-arg? term?)
      ;; Possessive macro.
      ((term? 's) noun?)
      ;; np+preds.
      (np+preds term? (+ pred?))
      ;; Object quoted expression.
      (|"| _+ |"|) ; same as (\" .. \"), but breaks the syntax highlighting less.
      ;; Fall back if np+preds arguments not analyzed correctly.
      (np+preds _+)
      ((_!1 's) _!2)
      
      ;; Rather than building a whole set of types corresponding to versions 
      ;; with the hole contained, I'll just check it dynamically.
      [*]h
      [*]s))

(defparameter *ttt-verb*
   '(! lex-verb?
       (pasv lex-verb?)
       (verb? (+ (! term? pred?)))
       (adv-a? verb?)
       (verb? (+ adv-a?))
       (aux? verb?)
       (verb? adv-a? term?)
       ((? verb?) lex-coord? (+ verb?))
       
       ;; Fall back if arguments not analyzed correctly.
       (verb? _!)
       ))

(defparameter *ttt-pred*
   '(! verb? noun? adj? tensed-verb? pp?
       (lex-rel? pred?)
       (sub lex-rel? sent?)
       
       ;; Fall back on arguments not analyzed correctly.
       (lex-rel? _!)
       (sub lex-rel? _!)
       ))

(defparameter *ttt-aux*
  '(! lex-aux? perf prog))

(defparameter *ttt-tensed-aux*
  '(lex-tense? aux?))

(defparameter *ttt-tensed-verb*
  '(! (lex-tense? verb?)
      (tensed-verb? (? (! term? pred?)) (! term? pred?))
      (tensed-aux? verb?)
      (adv-a? tensed-verb?)
      (tensed-verb? (+ adv-a?))
      (tensed-verb? adv-a? term?)))

(defparameter *ttt-det*
  '(! lex-det?
      (lex-detformer? adj?)))

(defparameter *ttt-sent*
  '(! (term? verb?)        ; subject verb
      (sent? lex-coord? (+ sent?)) ; multiple sentences
      (sent-mod? sent?)    ; sentence modifier, sentence
      (sent? sent-mod?)    ; sentence, sentence modifier
      (adv-a? term? verb?) ; action adverb, subject, verb
      (sent? sent-punct?))); sentence with punctuation 

(defparameter *ttt-tensed-sent*
  '(! ;; Simple sentence.
      (term? tensed-verb?)
      ;; Coordinated sentence.
      (tensed-sent? lex-coord? (+ tensed-sent?))
      ;; Modified sentence (e.g. curried coordination). 
      (sent-mod? tensed-sent?)
      ;; Postfixed sentence modification.
      (tensed-sent? sent-mod?)
      ;; Backwards sentence... 
      (tensed-verb? adv-a? term?)
      ;; Punctuated sentence.
      (tensed-sent? sent-punct?)
      ;; Prepositionally coordinated sentences.
      ((lex-ps? tensed-sent?) tensed-sent?)
      (tensed-sent? (lex-ps? tensed-sent?))
      ;; Inverted sentence.
      ((lex-tense? (!2 lex-verb? aux?)) term? verb?)
      ;; Phrasal utterances.
      (pu _!1)
      ;; Multiple sentences stuck together (e.g. some multi-sentence annotations).
      (tensed-sent? (+ tensed-sent?))
      ;; Expletives are treated as tensed sentences.
      lex-x?
      ;; Yes/no expressions are treated as tensed sentences.
      lex-yn?
      ;; Greetings.
      lex-gr?
      (gr _!)
      ))

(defparameter *ttt-sent-mod*
  '(!1 
      (lex-coord? (!2 tensed-sent? sent?))))

(defun noun? (x) (ttt:match-expr *ttt-noun* x))
(defun adj? (x) (ttt:match-expr *ttt-adj* x))
(defun adv-a? (x) (ttt:match-expr *ttt-adv-a* x))
(defun adv-e? (x) (ttt:match-expr *ttt-adv-e* x))
(defun adv-s? (x) (ttt:match-expr *ttt-adv-s* x))
(defun adv-f? (x) (ttt:match-expr *ttt-adv-f* x))
(defun pp? (x) (ttt:match-expr *ttt-pp* x))
(defun term? (x) (ttt:match-expr *ttt-term* x))
(defun verb? (x) (ttt:match-expr *ttt-verb* x))
(defun pred? (x) (ttt:match-expr *ttt-pred* x))
(defun det? (x) (ttt:match-expr *ttt-det* x))
(defun aux? (x) (ttt:match-expr *ttt-aux* x))
(defun tensed-aux? (x) (ttt:match-expr *ttt-tensed-aux* x))
(defun tensed-verb? (x) (ttt:match-expr *ttt-tensed-verb* x))
(defun sent? (x) (ttt:match-expr *ttt-sent* x))
(defun tensed-sent? (x) (ttt:match-expr *ttt-tensed-sent* x))
(defun sent-mod? (x) (ttt:match-expr *ttt-sent-mod* x))

(defun record-type? (x) (member x *record-types*))
(defun sent-punct? (x)
  (member x '(! ? .?)))

;; Reifiers.
(defun noun-reifier? (x)
  (member x '(k)))
(defun tensed-sent-reifier? (x)
  (member x '(that tht whether ans-to)))
(defun sent-reifier? (x)
  (member x '(ke)))
(defun verb-reifier? (x)
  (member x '(ka to gd)))

;; Operator forming type-shifters.
(defun advformer? (x)
  (member x '(adv-a adv-e adv-s adv-f)))
(defun detformer? (x)
  (member x '(fquan nquan)))

(defparameter *type-id-fns*
  (list (list #'noun? 'noun)
        (list #'adj? 'adj)
        (list #'adv-a? 'adv-a)
        (list #'adv-e? 'adv-e)
        (list #'adv-s? 'adv-s)
        (list #'adv-f? 'adv-f)
        (list #'pp? 'pp)
        (list #'term? 'term)
        (list #'verb? 'verb)
        (list #'pred? 'pred)
        (list #'det? 'det)
        (list #'aux? 'aux)
        (list #'tensed-aux? 'tensed-aux)
        (list #'tensed-verb? 'tensed-verb)
        (list #'sent? 'sent)
        (list #'tensed-sent? 'tensed-sent)
        (list #'lex-tense? 'tense)
        (list #'sent-punct? 'sent-punct)
        (list #'sent-mod? 'sent-mod)
        (list #'noun-reifier? 'noun-reifier)
        (list #'verb-reifier? 'verb-reifier)
        (list #'sent-reifier? 'sent-reifier)
        (list #'tensed-sent-reifier? 'tensed-sent-reifier)
        (list #'advformer? 'advformer)
        (list #'detformer? 'detformer)))

;; Hypothesizes the type of the given ULF formula.
(defun ulf-type? (x)
  (let ((matched (remove-if-not #'(lambda (pair) (apply (first pair) (list x)))
                                *type-id-fns*)))
    (if matched
      (mapcar #'second matched)
      '(unknown))))

;; Labels formula with the hypothesized types.
;; TODO: Merging this function with 'ulf-type?' to get all types in one bottom
;;       up fashion would speed this up a lot.
(defun label-formula-types (f)
  (cond 
    ((atom f) f)
      ;(list (cons 'types (ulf-type? f)) f))
    (t (list (cons 'types (ulf-type? f)) (mapcar #'label-formula-types f)))))

