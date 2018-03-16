(in-package :music)

(defmacro defsong (name-and-metadata &body body)
  "Define a song."
  (let ((name (if (symbolp name-and-metadata)
		  name-and-metadata
		  (car name-and-metadata)))
	(metadata (when (listp name-and-metadata)
		    (cdr name-and-metadata))))
    `(defparameter ,name
       (make-instance 'song
		      :body (list ,@body) ,@metadata))))

(defclass song ()
  ((title
    :type string
    :initarg :title
    :initform "Untitled"
    :accessor title)
   (composer
    :type string
    :initarg :composer
    :initform "Unknown"
    :accessor composer)
   (programmer
    :type string
    :initarg :programmer
    :initform "Unknown"
    :accessor programmer)
   (description
    :type string
    :initarg :description
    :initform "It's a song!"
    :accessor description)
   (body
    :type list ;; unsure yet
    :initarg :body
    :initform nil
    :accessor body))
  (:documentation "Represents a piece of music."))
