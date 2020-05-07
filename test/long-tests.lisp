;;; Gene Louis Kim, 5-6-2020
;;; Unit tests for long sentences

(in-package :ulf-sanity-checker)

(setq *print-failures* t)
(setq *print-errors* t)
(setq *print-summary* t)
(setq *summarize-results* t)

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

(define-test long-issue-sent2
  "Long sentence with unexpected UNKNOWN"
  (:tag :bugfix :long-sent)
  (let ((ulf
          '(((ALL.D (OF.P (THE.D ((PLUR DOG.N) (OF.P-ARG (HIS.D (PLUR FARM-YARD.N)))))))
               ((PAST FORM.V) (A.D (PACK.N (OF.P-ARG (K (PLUR HOUND.N)))))
                                 (ADV-E (AT.P (K NEED.N)))))
             ((HIS.D (PLUR GROOM.N)) ((PAST BE.V) (= (HIS.D (PLUR HUNTSMAN.N)))))
              (AND.ADV-S
                  ((THE.D (CURATE.N (OF.P-ARG (THE.D VILLAGE.N))))
                      ((PAST BE.V) (= (HIS.D (GRAND.A ALMONER.N))))))))
        (badulf 
          '(((ALL.D (OF.P (THE.D ((PLUR DOG.N) (OF.P-ARG (HIS.D (PLUR FARM-YARD.N)))))))
               ((PAST FORM.V) (A.D (PACK.N (OF.P-ARG (K (PLUR HOUND.N)))))
                                 (ADV-E (AT.P (K NEED.N)))))
             ((HIS.D (PLUR GROOM.N)) ((PAST BE.V) (= (HIS.D (PLUR HUNTSMAN.N)))))
              (AND.CC
                  ((THE.D (CURATE.N (OF.P-ARG (THE.D VILLAGE.N))))
                      ((PAST BE.V) (= (HIS.D (GRAND.A ALMONER.N)))))))))
    (assert-equal nil (sanity-check ulf :silent? t) ulf)
    (assert-equality #'(lambda (x y) (not (equal x y)))
                     nil
                     (sanity-check badulf :silent? t) badulf)))

(define-test long-issue-sent3
  "Long sentence with a few issues."
  (:tag :bugfix :long-sent)
  (let ((ulf
          '((HE.PRO
  ((PAST PERF)
   (PITCH.V (AS.PS (I.PRO ((PRES PERF) (SAY.V REF.PRO))))
    (ADV-A
     (AGAINST.P
      (NP+PREDS (THE.D (PLUR BULWARK.N))
       (SUB (ON.P WHICH.REL)
            (HE.PRO
             ((PAST LIE.V) *H
              (LIKE.A
               (SOME.D
                (N+PREDS (HORRIBLE.A (UNGAINLY.A (SORT-OF.N (K PUPPET.N))))
                 ((LIFE-SIZE.A INDEED.ADV-S)
                  (BUT.PS
                   (SUB
                    (HOW-EM.MOD-A
                     (DIFFERENT.A
                      (FROM.P-ARG
                       ((((K LIFE.N) 'S) COLOUR.N) OR.CC
                        (((K LIFE.N) 'S) COMELINESS.N)))))
                    (IT.PRO ((PRES BE.V) *H)))))))))))))))))
 !)))
    (assert-equal nil (sanity-check ulf :silent? t) ulf)))

