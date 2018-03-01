(defpackage music
  (:use :cl)
  (:export defsong
	   make-song
	   song-title
	   song-composer
	   song-programmer
	   song-body
	   letter-name
	   make-pitch-class
	   pitch-class-letter-name
	   pitch-class-accidental
	   make-note
	   note-pitch-class
	   note-octave))
