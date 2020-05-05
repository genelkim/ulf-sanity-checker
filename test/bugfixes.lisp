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

(define-test long-issue-sent1
  "Long sentence with multiple strange issues."
  (:tag :bugfix :long-sent)
  (let ((ulf
          '((((THE.D (PLUR TREE.N))
              ((PAST PERF)
               (BEND.V OVER.ADV-A (ADV-A (TOWARDS.P (THE.D (PLUR NETTLE.N)))))))
             ((THE.D PLANT.N) ((PAST PERF) (SPRING.V UPWARD.ADV-A)))
             ((THE.D BRANCH.N) ((PAST PERF) INCLINE.V))
             ((NP+PREDS THAT.PRO
               (WHICH.REL ((PRES CRAWL.V) (ADV-E (ON.P (THE.D EARTH.N))))))
              ((PAST PERF)
               (GO.V
                (ADV-A
                 (IN.P
                  (K
                   (N+PREDS SEARCH.N
                    (OF.P
                     (NP+PREDS THAT.PRO
                      (WHICH.REL
                       ((PRES EXPAND.V) (ADV-E (IN.P (THE-GEN.D AIR.N))))))))))))))
             ((NP+PREDS THAT.PRO
               (WHICH.REL ((PRES FLOAT.V) (ADV-E (ON.P (THE-GEN.D WIND.N))))))
              ((PAST PERF)
               (BEND.V OVER.ADV-A
                (ADV-A
                 (TOWARDS.P
                  (NP+PREDS THAT.PRO
                   (WHICH.REL ((PRES TRAIL.V) (ADV-E (IN.P (THE-GEN.D MOSS.N)))))))))))
             (((K (PLUR TRUNK.N)) (K (PLUR BOUGH.N)) (K (PLUR LEAF.N)) (K (PLUR FIBRE.N))
               (K (PLUR CLUSTER.N)) (K (PLUR TENDRIL.N)) (K (PLUR SHOOT.N))
               (K (PLUR SPINE.N)) AND.CC (K (PLUR THORN.N)))
              ((PAST PERF)
               ((MINGLE.V CROSS.V MARRY.V AND.CC CONFOUND.V) THEMSELVES.PRO
                (ADV-A (IN.P EACH_OTHER.PRO))))))
            ((K (N+PREDS VEGETATION.N (IN.P (A.D ((DEEP.A AND.CC CLOSE.A) EMBRACE.N)))))
             ((PAST PERF)
              ((CELEBRATE.V AND.CC ACCOMPLISH.V) THERE.ADV-E
               (ADV-E
                (UNDER.P
                 (THE.D (WELL-PLEASED.A (N+PREDS EYE.N (OF.P (THE.D |Creator.N|)))))))
               (ADV-E
                (IN.P
                 (THAT.D
                  (N+PREDS ENCLOSURE.N (((THREE.A HUNDRED.N) (PLUR FOOT.N)) SQUARE.N)
                   (= (THE.D (HOLY.A (N+PREDS MYSTERY.N (OF.P (K FRATERNITY.N))))))
                   (N+PREDS SYMBOL.N (OF.P (THE.D (HUMAN.N FRATERNITY.N))))))))))))))
	(assert-equal nil (sanity-check ulf :silent? t) ulf)))

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

