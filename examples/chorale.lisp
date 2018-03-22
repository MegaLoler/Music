(defpackage :chorale
  (:use :cl :music)
  (:export chorale))
(in-package :chorale)

(defparameter *chorale*
  (with-key 'c-major
    (seq (apply #'mapcar
		(cons #'list
		      (make-chorale
		       (harmony '(I IV64 V43 I6 I6 IV IV I64 I64 V V65/vi V/vi
				  vi visus2 iii6 iv iimin7 I iimin7 I6 IV I64 Vsus4 V7
				  Isus4 I)))))
	 '(1/2 1 1 1/3 1 1 1 2/3 2 1/2 2/3 2
	   1/2 1 1 1/3 1 1 1 1 1 1/2 1 1 1/4 1/4))))

;; (play *chorale*)
