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
            ((SUB DOWN.ADV-A ((THE.D CEILING.N) ((PAST COME.V) *H))) AND.CC
             ((THE.D DOG.N) ((PAST GO.V) AWAY.ADV-A)))
            )))
    (loop for ulf in ulfs
          do (assert-equal nil (sanity-check ulf :silent? t) ulf))))

(define-test quotes-as-pred-mod
  "Error raised on quotes with terms."
  (:tag :bugfix :quote-as-pred-mod)
  (let ((ulfs
          '(
            (THEY.PRO
						 (((PAST CALL.V) HIM.PRO (|"| (MY.D LORD.N) |"|)) AND.CC
						  ((PAST LAUGH.V) (AT.P-ARG (ALL.D (OF.P (HIS.D STORY.N)))))))
						(({the}.d (old.a |Fauchelevent|.n)) ((past return.v) (|"| Exactly.adv-s |"|)))
						(|"| (THE.D (|Petit-Picpus| CONVENT.N)) |"|)
            )))
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

(define-test preverb-flat-mod
  "Type UKNOWN when verb is premodified in a flat manner."
  (:tag :bugfix :verb-premod)
  (let ((ulfs
          '(
            ((SUB (ADV-A (WITH.P (A.D (PHILOSOPHICAL.A FLOURISH.N))))
                  (|Cato| ((PRES THROW.V) HIMSELF.PRO (ADV-A (UPON.P (HIS.D SWORD.N))) *H)))
             (I.PRO (QUIETLY.ADV-A (PRES TAKE.V) (TO.P-ARG (THE.D SHIP.N)))))
            )))
    (loop for ulf in ulfs
          do (assert-equal nil (sanity-check ulf :silent? t) ulf))))

(define-test predicate-modification-false-positive
  "Error message about pairing up predicate modification is over eager."
  (:tag :bugfix :pred-mod-false-positive)
  (let ((ulfs
          '(
						(((NQUAN (MORE-THAN.MOD-A ONE.A)) (N+PREDS DREAMER.N (OF.P (THAT.D EPOCH.N))))
						 OFTEN.ADV-F
						 ((PAST ALLOW.V) ((HIS.D (PLUR THOUGHT.N)) AND.CC (HIS.D (PLUR EYE.N)))
						  (TO
						   (PENETRATE.V INDISCREETLY.ADV-A
						    (ADV-A
						     (BETWEEN.P
						      (THE.D
						       (N+PREDS (PLUR BAR.N)
						        (OF.P
						         (NP+PREDS (THAT.D ((ANCIENT.A AND.CC PADLOCKED.A) GATE.N)) TWISTED.A
						          TOTTERING.A
						          (((PASV FASTEN.V)
						            (TO.P-ARG
						             (TWO.D ((GREEN.A AND.CC MOSS-COVERED.A) (PLUR PILLAR.N)))))
						           AND.CC
						           (ODDLY.ADV-A
						            ((PASV CROWN.V)
						             (ADV-A
						              (WITH.P
						               (A.D
						                (N+PREDS PEDIMENT.N
						                 (OF.P (K (UNDECIPHERABLE.A ARABESQUE.N))))))))))))))))))))
            )))
    (loop for ulf in ulfs
          do (assert-equal nil (sanity-check ulf :silent? t) ulf))))

(define-test coordinated-nps
  "UNKNOWN type in coordinated NP."
  (:tag :bugfix :np-coord)
  (let ((ulfs
          '(
            ((a.d (|Web|.n site.n))
             ((pres may.aux-s)
              (be.v created.a
                    (adv-a (in.p ((an.d intranet.n) or.cc
                                  (a.d ((local.a or.cc private.a) cloud.n)) or.cc
                                  (a.d ((virtual.a private.a) network.n))))))))
            )))
    (loop for ulf in ulfs
          do (assert-equal nil (sanity-check ulf :silent? t) ulf))))

(define-test coordinated-tensed-sents
  "UKNOWN type in coordinated tensed sentences."
  (:tag :bugfix :tensed-sents)
  (let ((ulfs
          '(
						(GRADUALLY.ADV-E
						 (((MY.D MIND.N) ((PAST COME.V) BACK.ADV-A AGAIN.ADV-S))
						  ((MY.D (PLUR PULSE.N))
						   ((PAST QUIET.V) DOWN.ADV-A
						    (ADV-A (TO.P (A.D ((MORE.MOD-A NATURAL.A) TIME.N))))))
						  AND.CC
						  (I.PRO
						   ((PAST BE.V) ONCE_MORE.ADV-S
						    (IN.P (K (POSSESSION.N (OF.P-ARG MYSELF.PRO))))))))
            )))
    (loop for ulf in ulfs
          do (assert-equal nil (sanity-check ulf :silent? t) ulf))))

