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

