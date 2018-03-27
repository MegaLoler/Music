(in-package :music)

(defmethod lily ((note note) &optional (stream t))
  "Print an absolute note in lilypond format."
  (lily (letter-name note) stream)
  (lily (chromatic-offset note) stream)
  (lily (octave note) stream))

(defmethod lily ((octave integer) &optional (stream t))
  "Print an absolute octave in lilypond format."
  (let ((offset (- octave 3)))
    (format stream "~v@{~A~:*~}" (abs offset) (if (plusp offset) #\' #\,))))
