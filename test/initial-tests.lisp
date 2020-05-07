;; Gene Louis Kim, 5-2-2020
;; Unit tests for files I had before.

(in-package :ulf-sanity-checker)

(setq *print-failures* t)
(setq *print-errors* t)
(setq *print-summary* t)
(setq *summarize-results* t)

(define-test no-error-sanity-tests
  "Ensure these ULFs don't raise errors"
  (:tag :initial :no-error)
  (let ((ulfs
          '((((three.d four.d or.cc six.d) (plur person.n)) (past leave.v))
            (((pres do.aux-s) you.pro (know.v me.pro)) ?)
            ((sub (sub How.pq
                       ((pres can.aux-v) I.pro
                                         (stop.v (adv-a (with.p her.pro))) *h))
                  (you.pro ((pres mean.v) (ans-to *h)))) ?)
            (I.pro ((past deliver.v) (the.d artifact.n)
                                     carefully.adv-a
                                     (to.p-arg (the.d curator.n))))
            (((fquan (= 0.6)) (plur man.n)) (pres sleep.v))
            ((sub when.adv-e ((past be.aux-v)
                              (the.d (first.a (flush.n toilet.n)))
                              ((pasv invent.v) *h))) ?)
            ((they.pro ((past (pasv trick.v)) (into.p-arg (ka go.v)))) ?)
            ; be.aux-v inverted passive
            (((past be.aux-v) they.pro ((pasv trick.v) (into.p-arg (ka go.v)))) ?)
            (you.pro
              ((pres must.aux-v)
               (not (lose.v
                      (k
                        (sight.n
                          (of.p-arg (your.d (goal.n (mod-n (in.p (k life.n))))))))))))
            (((past make.v) he.pro (a.d table.n)) ?)
            (you.pro ((pres be.v) (way.mod-a (off.p (the.d track.n)))))
            ((Who.pro ((past be.v) (= (the.d (1st.a | U.S. President|.n))))) ?)
            ; flattened args
            (I.pro ((past deliver.v) (the.d artifact.n)
                                     carefully.adv-a
                                     (to.p-arg (the.d curator.n))))
            (you.pro ((pres (pasv require.v)) (to (help.v them.pro))))
            (YOU.PRO
             ((PRES OUGHT_TO.AUX-S)
              (PERF (TAKE.V (((YOUR.D (FATHER-OF.N *S)) 'S) ADVICE.N)))))
            (pu cool.a)
            (|"| (Well.x (perhaps.adv-s (not {ref}.sent)))
             (qt-attr (|Alice| ((past say.v) *qt
            										(adv-a (in.p (a.d (soothing.a tone.n)))))))
             (({you}.pro ((pres do.aux-s) not
            							(be.v angry.a (adv-a (about.p it.pro))))) !) |"|)
            (|"| ((pres will.aux-s) not you.pro
            (qt-attr (he.pro ((past say.v) *qt
            									(adv-a (interrupt.v ((|John| 's) monologue.n))))))
            	(please.adv-s (get.v on.adv-a (with.p-arg it.pro)))) |"|)
            (((pres can.aux-v) you.pro
            	(break_away.v (from.p-arg (your.d (plur (parent-of.n *ref))))))
             ?)
            ((REP (*P ((PRES LET.V) US.PRO GO.V)) (K (PLUR DUDE.N))) !)
            (i.pro definitely.adv-s ((past give.v) him.pro (a.d present.n) yesterday.adv-e (adv-e (at.p (k school.n)))))
            (((((My.d printer.n) 's) (black.n ink.n)) ((pres be.v) defective.a))
             ((it.pro ((pres bleed.v) (adv-a (across.p (the.d page.n)))))
              (if.ps (I.pro ((pres touch.v) it.pro
                                            (adv-a (with.p (my.d finger.n))))))))
            (We.pro ((past eat.v)
                     (adv-e (at.p (a.d (n+preds restaurant.n
                                                (just.mod-a (outside.p |Boston|))))))))
            (|Tom|
              ((past do.aux-s) not
                               (know.v
                                 (whether
                                   (it-extra.pro (((past will.aux-s) (be.v (worth.a (his.d while.n))))
                                                  (ka (apply.v (for.p-arg (the.d job.n))))))))))
            ((sub What.pro
                  ((pres do.aux-s) (ds math-expression "7847+5943") (equal.v *h))) ?)
            ((((the.d ventriloquist.n) 's) (voice-of.n *s)) ((past repeat.v) (his.d distich.n)))
            ((sub (What.d (two.a (plur country.n)))
                  ((pres be.v) |Andorra| (nestled.a
                                           (mod-a (between.p *h))))) ?)
            ((voc |Mary|) (I.pro ((pres see.v) you.pro)))
            ((My.d (n+preds (ill.a (plur feeling.n))
                            (towards.p you.pro)))
             (voc |Lex|) ((pres be.v) endless.a))
            ((voc (np+preds you.pro rascal.n))
             (sub (at.p (what.d place.n)) ((pres perf) you.pro (be.v *h)))
             (voc |John|) ?)
            (YOU.PRO
              ((PRES SHOULD.AUX-V)
               ((ASK.V (YOUR.D (FATHER-OF.N *S)) (ADV-A (FOR.P (HIS.D ADVICE.N))))
                AND.CC (FOLLOW.V IT.PRO))))
            (I.pro ((pres will.aux-s) (go.v there.adv-e)))
            ((sub Why.adv-s
                  ((pres do.aux-s) not you.pro
                                   (give.v me.pro (a.d break.n) *h))) ?)
           )))
    (loop for ulf in ulfs
          do (assert-equal nil (sanity-check ulf :silent? t) ulf))))

(define-test yes-error-sanity-tests
  "Ensure these have some error"
  (:tag :initial :yes-error)
  (let ((ulfs
          '((((fquan (= 0.6.a)) (plur man.n)) (pres sleep.v))
            (((pres do.aux-v) you.pro (know.v me.pro)) ?)
            ; original inverted passives
            ((sub when.adv-e ((past be.aux-v)
                              (the.d (first.a (flush.n toilet.n)))
                              ((past (pasv invent.v)) *h))) ?)
            ((sub when.adv-e ((past (pasv invent.v)) (the.d (first.a (flush.n toilet.n))))) ?)
            ((sub when.adv-e ((past (pasv invent.v)) (the.d (first.a (flush.n toilet.n))) *h)) ?)
            (((past pasv) they.pro (trick.v (into.p-arg (ka go.v)))) ?)
            ; unflattened args
            (I.pro (((past deliver.v) (the.d artifact.n) carefully.adv-a)
                    (to.p-arg (the.d curator.n))))
            (I.pro (((past deliver.v) (the.d artifact.n))
                     carefully.adv-a (to.p-arg (the.d curator.n))))
            (I.pro ((((past deliver.v) (the.d artifact.n))
                     carefully.adv-a) (to.p-arg (the.d curator.n))))
            ; bad passive
            (you.pro ((pres pasv) require.v) (to (help.v them.pro)))
            (|"| (Well.x (perhaps.adv-s (not {ref}.sent)))
             (qt-attr (|Alice| (((past say.v))
            										(adv-a (in.p (a.d (soothing.a tone.n)))))))
             (({you}.pro ((pres do.aux-s) not
            							((be.v angry.a) (adv-a (about.p it.pro))))) !) |"|)
            (|"| ((pres will.aux-s) not you.pro
            (qt-attr he.pro (he.pro (((past say.v) *qt)
            									(adv-a (interrupt.v ((|John| |'s|) monologue.n))))))
            	(please.adv-s ((get.v on.adv-a) (with.p-arg it.pro)))) |"|)
            (|"| ((pres will.aux-s) not you.pro
                                     (qt-attr (he.pro (((past say.v) *qt)
                                                       (adv-a (interrupt.v ((|John| 's) monologue.n))))))
                                     (please.adv-s ((get.v on.adv-a) (with.p-arg it.pro)))) |"|)
            ((sub
               How.pq
               ((pres can.aux-v) I.pro
                                 ((find_out.v
                                    (((nquan (how.adv-a much.a)) (income.n tax.n))
                                     (((pres (pasv pay.v)) (on.p-arg (k ((|Social|.a |Security|.n) income.n))))
                                      (adv-e (on.p (the.d (|1998| (income.n tax.n)))))))) *h))) ?)
            (((That.rel ((pres be.v) (somewhat.adv-a ((past explain.v) (adv-a (at.p (the.d end.n)))))))) |.|)
            ((sub How.pq ((pres be.v) ((|Easter Sunday| 's) date.n) determined.a *h)) ?)
            (We.pro ((past eat.v)
                     (adv-e (at.p (a.d (n+preds restaurant.n
                                                (just.adv-a (outside.p |Boston|))))))))
            ((pres do.aux-s) you.pro really.adv-a ((pres want.v) that.pro) ?)
            (I.pro ((pres will.aux-v) (go.v there.adv-e)))
            ; Name splitting
            ((Who.pro ((past be.v) (= (the.d (1st.a (| U.S.| |President|.n)))))) ?)
            ((voc (n+preds you.pro rascal.n))
             (sub (at.p (what.d place.n)) ((pres perf) you.pro (be.v *h)))
             (voc |John|.n) ?)
            )))
    (loop for ulf in ulfs
          do (assert-true (not (equal
                                 nil
                                 (sanity-check ulf :silent? t)))
                          ulf
                          (sanity-check ulf :silent? t)))))

(define-test large-example-tests
  "Test large examples"
  (:tag :initial :large)
  (let ((time-limit 10)
        runtime
        (ulfs
          '(
 ((IT-EXTRA.PRO
  (((PRES BE.V) TRUE.A)
   (THT
    ((A.D
      (N+PREDS CHILD.N
       (JUST.ADV-E
        ((PASV DROP.V) (ADV-A (FROM.P (ITS.D (DAM-OF.N *S))))))))
     ((PRES MAY.AUX-S)
      ((PASV SUPPORT.V) (BY.P-ARG (HER.D MILK.N))
       (ADV-E (FOR.P (A.D (SOLAR.A YEAR.N))))
       (ADV-A
        (WITH.P
         (NP+PREDS (K (LITTLE.A (OTHER.A NOURISHMENT.N)))
          ((MOD-A (AT.P (THE.D (MOST.A REF.N))))
           (NOT (ABOVE.P
                 (THE.D
                  (VALUE-OF.N
                   (NP+PREDS (TWO.D (PLUR SHILLING.N))
                    (SUB WHICH.REL
                         ((THE.D (MOTHER-OF.P *REF))
                          ((PRES MAY.AUX-S) CERTAINLY.ADV-S
                           (GET.V
                            (*H OR.CC
                             (THE.D
                              (N+PREDS VALUE.N
                               (IN.P (K (PLUR SCRAP.N))))))
                            (ADV-A
                             (BY.P
                              (HER.D
                               (N+PREDS
                                (LAWFUL.A OCCUPATION.N)
                                (OF.P
                                 (KA BEG.V)))))))))))))))))))))))))
 (AND.ADV-S
  ((IT-CLEFT.PRO
    (((PRES BE.V)
      (ADV-E
       (EXACTLY.ADV-S (AT.P (KA (BE.V ((ONE.D YEAR.N) OLD.A)))))))
     (THAT.REL
      (I.PRO
       ((PRES PROPOSE.V)
        (TO
         (PROVIDE.V (FOR.P-ARG THEM.PRO)
          (ADV-A (IN.P (SUCH.D (= (A.D MANNER.N))))))))))))
   AS.CC
   ((ADV-S
     (INSTEAD_OF.P
      ((KA
        (BE.V
         (A.D
          (N+PREDS CHARGE.N
           (UPON.P
            ((THEIR.D (PLUR (PARENT-OF.N *S))) OR.CC
             (THE.D PARISH.N)))))))
       OR.CC
       (KA
        (WANT.V ((K FOOD.N) AND.CC (K RAIMENT.N))
         (ADV-E
          (FOR.P
           (THE.D
            (N+PREDS REST.N (OF.P (THEIR.D (PLUR LIFE.N))))))))))))
    (THEY.PRO (PRES SHALL.AUX-S)
     ((ADV-S (ON.P (THE.D CONTRARY.N)))
      (CONTRIBUTE.V
       ((TO.P-ARG (THE.D FEEDING.N)) AND.CC
        (PARTLY.ADV-S
         (TO.P-ARG
          (THE.D
           (CLOATHING.N
            (OF.P-ARG (MANY.D (PLUR THOUSAND.N)))))))))))))))
((IT.PRO
  ((PRES BE.V) (A.D (MELANCHOLY.A OBJECT.N))
   (ADV-A
    (TO.P
     (NP+PREDS THOSE.PRO
      (WHO.REL
       (((PRES WALK.V) (ADV-E (THROUGH.P (THIS.D (GREAT.A TOWN.N)))))
        OR.CC ((PRES TRAVEL.V) (IN.P (THE.D COUNTRY.N))))))))))
 (WHEN.PS
  (THEY.PRO
   ((PRES SEE.V)
    ((THE.D (PLUR STREET.N)) (THE.D (PLUR ROAD.N)) AND.CC
     (K (PLUR CABBIN-DOOR.N)))
    (CROWDED.A
     (MOD-A
      (WITH.P
       ((K
         (N+PREDS (PLUR BEGGAR.N) (OF.P (THE.D (FEMALE.A SEX.N)))
          (((PASV FOLLOW.V)
            (BY.P-ARG
             (NP+PREDS ((THREE.D FOUR.D OR.CC SIX.D) (PLUR CHILD.N))
              (ALL.MOD-A (IN.P (K (PLUR RAG.N)))))))
           AND.CC
           (IMPORTUNE.V (EVERY.D PASSENGER.N)
            (ADV-A (FOR.P (AN.D ALMS.N)))))))))))))))
(|Young| ((past say.v)
					(tht
					 ((the.d |FDA|.n)
						(rep
						 ((pres prog) (receive.v
							 (an.d (n+preds average.n (of.p (10.d (n+preds (plur application.n) *p)))))
							 (adv-a (= (each.d month.n)))))
						 (request.v (k
												 (n+post permission.n
													(to.p-arg (begin.v (k (clinical.a (n+post (plur trial.n)
																														 (for.p-arg (a.d (new.a drug.n)))
																														 (= (the.d (first.a (n+preds step.n
																																								 (in.p (ka (get.v (the.d drug.n) (approved.a (for.p-arg (k (prescription.n (n+post use.n
																																																																														(by.p-arg (the.d public.n)))))))))))))))))))))))))))
(((adv-s (For.p (k example.n)))
 (sub what.pro
      (exactly.adv-s
       ((past do.aux-s) (the.d | CIA|.n)
      ((tell.v (set-of |Major Giroldi| (his.d (fellow.a (coup.n (plur plotter.n))))))
         (np+preds *h
             (about.p
                    (set-of (k (| U.S.| (plur law.n)))
                        (k (n+preds (executive.a (plur order.n))
                                    (on.p (k (plur assassination.n))))))))))))) ?)
((sub (What.d part.n)
 ((past do.aux-s)
  (k (| U.S.| (plur warning.n)))
  ((play.v *h)
   (in.p-arg
    (((the.d major.n) 's)
	 (unwillingness.a
	  (to ((pull.v (the.d trigger.n))
		   (adv-e (sub (at-time.p which.rel)
                   	   ((he.pro
                         (((((past have.v) |General Noriega|) (in.p-arg (k custody.n)))
                           but.cc
                           ((past be.v)
                            (under.p (k (attack.n
                                         (by.p-arg
                                          (k ((pro.p |Noriega|) (plur troop.n)))))))))
                          (adv-e *h)))))))))))))) ?)
(((What.d (| U.S.| vice-president.n))
  once.adv-e
  ((past declare.v)
  (|"| ((If.ps (you.pro ((pres perf) (see.v (one.d slum.n)))))
       (you.pro ((pres perf) (see.v (them.d (all.a {ref}.n)))))) |"|))) ?)
((sub (|"| (now.adv-e
((sub what.pro
 (((pres prog) pester.v) (me.pro for.p) *h)) ?)) |"|))
 ((past cry.v) |Denisov| (suddenly.adv-e ((adv-a (lose.v his.pro temper.n)))) *h))
((k Logic.n)
 ((pres be.v)
  (= (a.d (systematic.a
           (method-of.n (k (come.v (to.p-arg (the.d (wrong.a conclusion.n)))
                                    (adv-a (with.p (k confidence.n)))))))))))
((now.adv-s ((pres can.aux-s) you.pro (tell.v me.pro (whether (it.pro ((pres be.v) ((adv-a actually.a) true.a) (that (|Mr. Rochester| ((pres perf) (ask.v you.pro (to (marry.v him.pro)))))))))))) ?)
(AND.ADV-S
 ((AS.PS
   ((ADV-E (AT.P (THIS.D MOMENT.N)))
    ((THE.D ORCHESTRA.N)
     ((PAST GIVE.V)
      (THE.D (N+PREDS SIGNAL.N (FOR.P (THE.D WALTZ.N))))))))
  (|Albert|
   (((PAST PUT.V) (HIS.D ARM.N)
     (ROUND.P (THE.D (N+PREDS WAIST.N (OF.P (THE.D COUNTESS.N))))))
    AND.CC
    ((PAST DISAPPEAR.V) (ADV-A (WITH.P HER.PRO))
     (ADV-A
      (IN.P (THE.D (N+PREDS WHIRL.N (OF.P (K (PLUR DANCER.N))))))))))))
 )))
    (loop for ulf in ulfs
          do (multiple-value-bind
               (result runtime)
               (util:timing #'(lambda () (sanity-check ulf :silent? t)))
               (declare (ignore result))
               (assert-true (< runtime time-limit)
                            ulf runtime time-limit)))))

