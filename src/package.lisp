(defpackage :music
  (:use :cl)
  (:export diatonic-to-chromatic-value
	   add-diatonic-values
	   subtract-diatonic-values
	   diatonic-mod
	   diatonic-class
	   chromatic-class

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
	   make-pitch-class
	   print-object
	   make-note
	   note
	   octave
	   octave-chromatic-offset
	   octave-diatonic-offset
	   above
	   below

	   quality-type
	   perfect perf p
	   major maj ma mj
	   minor min mi mn
	   augmented aug a
	   diminished dim d

	   quality
	   augmented-diminished-quality
	   make-quality
	   perfect-quality-from-chromatic-offset
	   major-minor-quality-from-chromatic-offset
	   quality-from-chromatic-offset
	   print-quality-type
	   make-quality
	   interval
	   make-interval
	   operate
	   sum
	   difference
	   reduce

	   key
	   mode
	   scale-degree
	   tonic
	   supertonic
	   mediant
	   subdominant
	   dominant
	   submediant
	   subtonic
	   leading-tone
	   major
	   minor
	   harmonic-minor
	   melodic-minor
	   ionian
	   dorian
	   phrygian
	   lydian
	   mixolydian
	   aeolian
	   locrian
	   make-key
	   scale
	   parallel
	   relative

	   a b c d e f g ♯ 𝄪 ♭ 𝄫 ♮))
