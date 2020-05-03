;;; Gene Louis Kim, 5-2-2020
;;; Unit tests for bug fixes

(in-package :ulf-sanity-checker)

(setq *print-failures* t)
(setq *print-errors* t)
(setq *print-summary* t)
(setq *summarize-results* t)

(define-test unknown-sub-type
  "Unknown type after apply 'sub' macro"
  (:tag :bugfix :unknown-sub)
  (let ((ulfs
          '(
						(SUB
							(|"| ((PU WHAT.PRO) ?) ((HE.PRO ((PAST TAKE.V) THEM.PRO)) ?)
								|"|)
							(HE.PRO ((PAST SAY.V) *H (ADV-A (WITH.P (K (SLIGHT.A EMOTION.N)))))))
						(SUB (|"| (PU (VERY.MOD-A WELL.A)) |"|) (HE.PRO ((PAST SAY.V) *H)))
						(SUB
							(|"|
								(YOU.PRO
									(((PRES TAKE.V) OUT.ADV-A (THE.D (PLUR HORSE.N)) QUICKLY.ADV-A) AND.CC
									 ((PRES COME.V) UP.ADV-A (ADV-A (TO.P ME.PRO)))))
								|"|)
							(|Andrea| ((PAST SAY.V) *H (ADV-A (TO.P (HIS.D GROOM.N))))))
						(SUB (ADV-A (HAVE.V (A.D (SLIGHT.A HEADACHE.N))))
								 (I.PRO ((PAST GO.V) (TO.P-ARG (K BED.N)) EARLY.ADV-E *H)))
            )))
    (loop for ulf in ulfs
          do (assert-equal nil (sanity-check ulf :silent? t) ulf))))

(define-test quotes-as-pred-mod
  "Error raised on quotes with terms."
  (:tag :bugfix :quote-as-pred-mod)
  (let ((ulfs
          '((THEY.PRO
						 (((PAST CALL.V) HIM.PRO (|"| (MY.D LORD.N) |"|)) AND.CC
						  ((PAST LAUGH.V) (AT.P-ARG (ALL.D (OF.P (HIS.D STORY.N))))))))))
		(loop for ulf in ulfs
          do (assert-equal nil (sanity-check ulf :silent? t) ulf))))

(define-test ds-string-bug
  "Domain specific pattern fails from the string."
  (:tag :bugfix :ds-string)
  (let ((ulfs
          '(
            (sub (The.d (n+preds (plur number.n)
                                 (present.v (by.p-arg (the.d (non-partisan.a
                                                               (congressional.a agency.n)))))))
                 ((if.ps (*h ((pres {be}.v) accurate.a)))
                  ({they}.pro ((pres would.aux-s)
                               (make.v it.pro
                                       (adv-a ((mod-a (even.mod-a more.a))
                                               (difficult.a
                                                 (mod-a (for.p (|Bush| and.cc |Congress|)))
                                                 (to (meet.v
                                                       (((the.d (|Gramm-Rudman| ((balanced.a budget.n) law.n))) 's)
                                                        (|1990| (n+preds (deficit.n target.n) (of.p (ds currency "$100 billion")))))))))))))))
            )))
		(loop for ulf in ulfs
          do (assert-equal nil (sanity-check ulf :silent? t) ulf))))

