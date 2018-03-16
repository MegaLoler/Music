(defpackage :music
  (:use :cl)
  (:export defsong
	   song
	   title
	   composer
	   programmer
	   description
	   body

	   letter-name
	   letter-name-p
	   accidental
	   accidental-p
	   chromatic-value
	   diatonic-value
	   chromatic-offset
	   print-chromatic-offset
	   pitch-class
	   print-object
	   note
	   octave
	   octave-midi-value
	   midi-value

	   a b c d e f g ♯ 𝄪 ♭ ♮))
