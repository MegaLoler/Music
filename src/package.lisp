(defpackage :music
  (:use :cl :sb-ext) ;; being sbcl dependant rn
  (:export diatonic-to-chromatic-value
	   add-diatonic-values
	   subtract-diatonic-values
	   diatonic-mod
	   diatonic-class
	   chromatic-class
	   rotate-left
	   rotate-right

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
	   note-or-pitch-class
	   nearest
	   realize

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
	   full-scale
	   parallel
	   relative
	   solfège-syllable

	   harmony
	   make-harmony
	   build-harmony
	   invert
	   
	   do di ra re ri me mi fa fi se sol si le la li te ti
	   a b c d e f g ♯ 𝄪 ♭ 𝄫 ♮ |#| b is es

	   musical-rest rest r
	   play-note
	   play-notes
	   note-on
	   note-off
	   notes-on
	   notes-off
	   panic
	   schedule
	   init-midi
	   close-midi
	   ensure-midi
	   play

	   environment
	   default-environment
	   event
	   container
	   chord
	   seq
	   v h
	   tempo
	   meter
	   make-tempo
	   make-meter
	   realize-duration
	   duration
	   on-time
	   off-time
	   realize-on-time
	   realize-off-time
	   sum-beat-values
	   cat
	   concat))
