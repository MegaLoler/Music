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
	   secondary
	   degree

	   harmony
	   make-harmony
	   build-harmony
	   invert
	   intervals

	   i ii iii iv v vi vii
	   do di ra re ri me mi fa fi se sol so si le la li te ti
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
	   concat

	   major minor augmented diminished
	   dominant major minor-major minor augmented-major augmented half-diminished diminished
	   major dominant minor-major minor augmented-major augmented half-diminished half-diminished-minor diminished minor-diminished
	   dominant major minor major minor augmented-major augmented half-diminished diminished
	   major dominant minor-major minor augmented-major augmented half-diminished
	   maj ma mj j Δ min mi mn − - aug au ag a + dim di dm d o ° dom half-dim half-di half-dm half-d h-dim h-di h-dm h-d hdim hdi hdm hd min-maj mi-ma mn-mj mn-j m-m m-j minmaj mima mnmj mnj mj mm aug-maj au-ma ag-j ag-j a-m a-j augmaj auma agmj agj am aj
	   sus2 sus4
	   add2 add4 add9 add11 add13))
