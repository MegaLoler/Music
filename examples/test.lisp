(defpackage test-song
  (:use :cl :music)
  (:export some-song
	   some-song2))
(in-package test-song)

;; just some tests with defsong

(defsong (some-song :composer "MegaLoler"
		    :programmer "Megaloler"
		    :title "Some Song"
		    :description "This is just a song dw bout it")
    (make-note))


(defsong some-song2
  (make-note))
