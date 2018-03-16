(defpackage :music
  (:use :cl)
  (:export diatonic-to-chromatic-value

	   defsong
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

	   quality-type
	   perfect perf p
	   major maj ma mj
	   minor min mi mn
	   augmented aug a
	   diminished dim d

	   quality
	   augmented-diminished-quality
	   make-quality
	   print-quality-type
	   make-quality
	   interval

	   a b c d e f g ♯ 𝄪 ♭ 𝄫 ♮))
