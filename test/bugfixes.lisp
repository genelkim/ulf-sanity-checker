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
						(|"|
						 (|"|
						  ((You.pro ((pres know.v) (ans-to (sub (at-loc.p which.rel) (I.pro ((pres live.v) *h))))))
						   (I.pro ((pres expect.v) you.pro ({at-time}.p (k (n+preds (tomorrow.pro morning.n)
                                                                        (at.p (ds date-time "nine o'clock"))))))))
						  |"|)
						|"|)
						((Well.x
						 (you.pro ((past give.v) him.pro
						           (the.d (n+preds (= (ds currency "two hundred francs"))
						                           (sub tht.rel
						                                (I.pro ((past perf) (leave.v *h (adv-a (for.p him.pro))))))))))) ?)
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

(define-test coordinated-adjective
  "UNKNOWN type in coordinated adjective phrase."
  (:tag :bugfix :adj-coord)
  (let ((ulfs
          '(
            (I.PRO
             ((PAST BE.V) HOWEVER.ADV-S
              (GOOD.A CLEVER.A COMPOSED.A AND.CC FIRM.A)
              (ADV-A (LIKE.P HIM.PRO))))
            )))
    (loop for ulf in ulfs
          do (assert-equal nil (sanity-check ulf :silent? t) ulf))))

(define-test paren-failure
  "Type checking failure on parentheses."
  (:tag :bugfix :paren)
  (let ((ulfs
          '(
((SHE.PRO
  ((PAST BE.V)
   ((HASTY.A BUT.CC GOOD-HUMOURED.A)
    (VAIN.A
     (|(|
      ((SHE.PRO ((PAST CAN.AUX-V) NOT (HELP.V IT.PRO)))
       (WHEN.PS
        ((EVERY.D (N+PREDS GLANCE.N (IN.P (THE-GEN.D GLASS.N))))
         ((PAST SHOW.V) HER.PRO
          (= (SUCH.D (= (A.D (FLUSH-OF.N (K LOVELINESS.N))))))))))
      |)|)
     BUT.CC (NOT AFFECTED.A))
    LIBERAL-HANDED.A
    (INNOCENT.A (OF.P-ARG (THE.D (N+PREDS PRIDE.N (OF.P (K WEALTH.N))))))
    INGENUOUS.A (SUFFICIENTLY.MOD-A INTELLIGENT.A) GAY.A LIVELY.A AND.CC
    UNTHINKING.A)))
 ((SHE.PRO
   ((PAST BE.V)
    ((VERY.MOD-A CHARMING.A) IN_SHORT.ADV-S
     (EVEN.ADV-S
      (TO.P-ARG
       (NP+PREDS
        (A.D
         (N+PREDS (COOL.A (OBSERVER-OF.N *REF)) (OF.P (HER.D (OWN.A SEX.N)))))
        (LIKE.P ME.PRO)))))))
  BUT.CC
  (SHE.PRO
   ((PAST BE.V) NOT
    ((PROFOUNDLY.MOD-A INTERESTING.A) OR.CC (THOROUGHLY.MOD-A IMPRESSIVE.A))))))
              )))
    (loop for ulf in ulfs
          do (assert-equal nil (sanity-check ulf :silent? t) ulf))))

(define-test interjection-unknown
  "Interjections don't seem to be handled in the sanity checker type system."
  (:tag :bugfix :interjection-unknown)
  (let ((ulfs
          '(
            (SUB (|"| (AH.X (GOOD.X GOD.X)) |"|) (|Fauchelevent| ((PAST CRY.V) *H)))
            )))
    (loop for ulf in ulfs
          do (assert-equal nil (sanity-check ulf :silent? t) ulf))))

(define-test named-predicates-as-term
  "Named predicates get analyzed as terms."
  (:tag :bugfix :named-predicate)
  (let ((ulfs
          '(
            ((THE.D RAIL-LINE.N)
              ((PAST (PASV CARRY.V)) (ADV-A (UNDER.P (THE.D |Tsugaru Strait.N|)))
                                     (ADV-A (TO.P |Hokkaido|))))
            (((THE.D |Milky Way.N|)
                ((PRES BE.V)
                    (= (A.D (VAST.A (BELT.N (OF.P-ARG (DISTANT.A (K (PLUR STAR.N))))))))))
              ((EACH.D STAR.N)
                 ((PRES BE.V) (= (A.D (N+PREDS SUN.N (LIKE.P (OUR.D ONE.N))))))))
            )))
    (loop for ulf in ulfs
          do (assert-equal nil (sanity-check ulf :silent? t) ulf))))

(define-test voc-unknown
  "Vocatives result in UNKNOWN types."
  (:tag :bugfix :voc)
  (let ((ulfs
          '(
						(|"|
						 ((VOC
						   (MY.D
						    (((VERY.MOD-A GOOD.A) (FRIEND-OF.N *S)) AND.CC
						     (EXCELLENT.A (NEIGHBOR-OF.N *S)))))
						  (QT-ATTR
						   ((THE.D COUNT.N) ((PAST REPLY.V) *QT (ADV-A (WITH.P (A.D SMILE.N))))))
						  YOU.PRO
						  (REALLY.ADV-A ((PRES EXAGGERATE.V) (MY.D (TRIFLING.A (PLUR EXERTION.N))))))
						 |"|)
						(|"| (YES.YN (VOC (YOUR.D EXCELLENCY.N))) |"|)
						(|"| ((I.PRO ((PRES HAVE.V) (THAT.D HONOR.N))) (VOC (YOUR.D EXCELLENCY.N))) |"|)
						((I.PRO ((PRES PERF) JUST.ADV-S (COMMIT.V (A.D WRONG.N)))) (VOC |sir|))
						(((BUT.ADV-S
               (ADV-S (FOR.P (TO (COME.V (ADV-A (TO.P (THE.D (POINT-OF.N *REF))))))))
						   (SUB (ADV-A (BY-MEANS-OF.P (NP+PREDS WHICH.PRO (= (THE.D DEUCE.N)))))
						        ((PAST DO.AUX-S) YOU.PRO
						         (MANAGE.V (TO (GET.V (IN.P-ARG HERE.PRO))) *H))))
						  (VOC (NP+PREDS YOU.PRO (= |Father Madeleine|))))
             ?)
						(|"| ((voc |Father Fauchelevent|)
						      (I.pro ((past save.v) (your.d life.n)))) |"|)
          )))
  (loop for ulf in ulfs
        do (assert-equal nil (sanity-check ulf :silent? t) ulf))))

(define-test inverted-perfprog
  "False positives with inverted perf constructions."
  (:tag :bugfix :inverted-perf)
  (let ((ulfs
          '(
            (((pres perf) you.pro not (learn.v (to (respect.v (plur other.n))))) ?)
(((PRES PERF) YOU.PRO
  (HAVE.V
   (ANY.D
    (N+PREDS EXPERIENCE.N (WITH.P (THIS.D (KIND-OF.N (K WORK.N))))))))
 ?)
(((PRES PERF) NOT YOU.PRO (GOT.V (ANY.D MONEY.N))) ?)

            (((PRES PROG) YOU.PRO
                          (REF.V (ADV-A ((IN.P (K JEST.N)) OR.CC (IN.P (K EARNEST.N))))))
             ?)
            (((PRES PROG) YOU.PRO (WRITE.V (A.D LETTER.N))) ?)
            ((SUB (ADV-E (AT.P (WHAT.D TIME.N)))
                        ((PRES PROG) YOU.PRO (GO.V (ADV-A (ON.P (K DUTY.N))) *H)))
              ?)
            ((SUB WHAT.PRO ((PRES PROG) YOU.PRO (LOOK.V (ADV-A (FOR.P *H))))) ?)
            ((SUB WHAT.PRO ((PRES PROG) YOU.PRO (LOOK.V (ADV-A (AT.P *H))))) ?)
            )))
    (loop for ulf in ulfs
          do (assert-equal nil (sanity-check ulf :silent? t) ulf))))

(define-test inverted-verbs
  "Issues with inverted verb constructions."
  (:tag :bugfix :inverted-verb)
  (let ((ulfs
          '(
            ; System confuses the predicative verb as an inverted sentence.
            ((YOU.PRO
              ((PRES LET.V) US.PRO
               (RETURN.V
                (TO.P-ARG
                 (THE.D
                  (N+PREDS EXPLANATION.N
                   (SUB WHICH.REL
                        (YOU.PRO ((PRES PERF) (ASK.V *H (OF.P-ARG ME.PRO)))))))))))
             !)
						(PLEASE.ADV-S
						 ((YOU.PRO
						   ((PRES LET.V) ME.PRO
						    (KNOW.V (ANS-TO (SUB WHAT.PRO (YOU.PRO ((PRES WANT.V) *H)))))))
						  !))
						(I.PRO
						 ((PAST FEEL.V) (MY.D HEART.N) POUND.V
						  (ADV-E (AFTER.P (KA (RUN.V A_LITTLE.ADV-A))))))
            )))
    (loop for ulf in ulfs
          do (assert-equal nil (sanity-check ulf :silent? t) ulf))))

(define-test it-cleft-issues
  "it-clefts just causes a whole lot of problems."
  (:tag :bugfix :it-cleft)
  (let ((ulfs
          '(
((SUB WHAT.PRO ((PAST BE.V) IT-CLEFT.PRO (= *H)
  (THAT.REL
   ((PRES DETERMINE.V)
    (THE.D
     (N+PREDS ROUTE.N (SUB THT.REL ((A.D RAILROAD.N) ((PRES TAKE.V) *H)))))))))
 ?)
            ))
        (badulfs
          '(
; it-cleft requires the parenthetical to appear after the "be"-phrase.
((REP (SUB (NP+PREDS WHAT.PRO *P) ((PAST BE.V) IT-CLEFT.PRO (= *H)))
  (THAT.REL
   ((PRES DETERMINE.V)
    (THE.D
     (N+PREDS ROUTE.N (SUB THT.REL ((A.D RAILROAD.N) ((PRES TAKE.V) *H))))))))
 ?)
            )))
    (loop for ulf in ulfs
          do (assert-equal nil (sanity-check ulf :silent? t) ulf))
    (loop for ulf in badulfs
          do (assert-equality #'(lambda (x y) (not (equal x y)))
                              nil
                              (sanity-check ulf :silent? t) ulf))))

(define-test it-extra-issues
  "it-extra.pro causes a lot of problems."
  (:tag :bugfix :it-extra)
  (let ((ulfs
          '(
            (IT-EXTRA.PRO (((PRES BE.V) (HOW.MOD-A THOUGHTLESS.A))
                           (KE (YOU.PRO (DO.V THAT.PRO)))))
            )))
    (loop for ulf in ulfs
          do (assert-equal nil (sanity-check ulf :silent? t) ulf))))

(define-test inverted-be
  "Inverted be.v raising errors."
  (:tag :bugfix :inverted-be)
  (let ((ulfs
          '(
            (((PRES BE.V) YOU.PRO (CONCERNED.A (WITH.P-ARG (K (PLUR POLITIC.N)))))
             ?)
            (((pres be.v) you.pro (sure.a (of.p-arg (your.d (plur fact.n))))) ?)
						(((PRES BE.V) NOT YOU.PRO HAPPY.A) ?)
            )))
    (loop for ulf in ulfs
          do (assert-equal nil (sanity-check ulf :silent? t) ulf))))

(define-test possible-ps-issue
  "Possible issue with ps."
  (:tag :bugfix :ps)
  (let ((ulf '(It.pro ((pres perf)
                       (be.v
                         (= (ten.d (plur year.n)))
                         (since.ps (we.pro (last.adv-e (past meet.v)))))))))
    (assert-equal nil (sanity-check ulf :silent? t) ulf)))

(define-test rel-in-sent-mod
	"Relativized sentence is not recognized when the relativizer is within a sentence modifying operator."
  (:tag :bugfix :rel-in-sent-mod :rel :sent-mod)
  (let ((ulfs
          '(
						(((PRES PERF) YOU.PRO EVER.ADV-S
						            (VISIT.V
						             (THE.D
						              (N+PREDS OFFICE.N
						               (SUB (AT-LOC.P WHICH.REL)
						                    ((YOUR.D (FATHER-OF.N *S)) (PRES WORK.V) (ADV-E *H)))))))
						           ?)
						(((PRES PERF) YOU.PRO
						  (VISIT.V
						   (THE.D
						    (N+PREDS TOWN.N
						     (SUB (AT-LOC.P WHICH.REL)
						          ((YOUR.D (FATHER-OF.N *S))
						           ((PAST PERF) BEAR.V (ADV-E *H))))))))
						 ?)
            )))
    (loop for ulf in ulfs
          do (assert-equal nil (sanity-check ulf :silent? t) ulf))))

