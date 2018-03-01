(in-package :music)

(defstruct song
  "Represents a song."
  (title "Untitled" :type string)
  (composer "Unknown" :type string)
  (programmer "Unknown" :type string)
  (description "A song" :type string)
  (body nil :type list))

(defmacro defsong (name-and-metadata &body body)
  "Define a song."
  (let ((name (if (symbolp name-and-metadata)
		  name-and-metadata
		  (car name-and-metadata)))
	(metadata (when (listp name-and-metadata)
		    (cdr name-and-metadata))))
    `(setf (get ',name 'song)
	   (make-song :body (list ,@body) ,@metadata))))

(defun song (song)
  "Get a song designated by `song'."
  (if (song-p song)
      song
      (get song 'song)))
