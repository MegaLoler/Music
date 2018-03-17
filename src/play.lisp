(in-package :music)

;; TODO
;;   get a real time midi library and make (play note) play notes!
;;   automatic velocity/accent distribution

(deftype musical-rest ()
  "Symbols representing a musical rest."
  `(member rest r))

(defvar *duration* 1)
(defvar *velocity* 60)
(defvar *unaccent* 4/5)
(defvar *reference* (note 'c4))
(defvar *key* (key 'c-major))

(defmethod play
    ((note note)
     &optional
       (time 0)
       (duration *duration*)
       (velocity *velocity*)
       (key *key*)
       (reference *reference*))
  "Play a note."
  (declare (ignore key reference))
  (format t "Playing note ~A at time ~A for ~As with velocity ~A.~%"
	  (chromatic-value note) time duration velocity)
  note)

(defmethod play
    ((pitch-class pitch-class)
     &optional
       (time 0)
       (duration *duration*)
       (velocity *velocity*)
       (key *key*)
       (reference *reference*))
  "Play a pitch class."
  (play (nearest pitch-class reference)
	time duration velocity key reference))

(defmethod play
    ((degree symbol)
     &optional
       (time 0)
       (duration *duration*)
       (velocity *velocity*)
       (key *key*)
       (reference *reference*))
  "Play a scale degree."
  (if (typep degree 'musical-rest)
      reference
      (play (nearest (resolve degree key) reference)
	    time duration velocity key reference)))

(defmethod play
    ((degree integer)
     &optional
       (time 0)
       (duration *duration*)
       (velocity *velocity*)
       (key *key*)
       (reference *reference*))
  "Play a scale degree."
  (play (nearest (resolve degree key) reference)
	time duration velocity key reference))

(defmethod play
    ((notes list)
     &optional
       (time 0)
       (duration *duration*)
       (velocity *velocity*)
       (key *key*)
       (reference *reference*))
  "Play a chord."
  (loop
     :for note :in notes
     :do (setf
	  reference
	  (play note time duration velocity key reference)))
  reference)

(defmethod play
    ((notes vector)
     &optional
       (time 0)
       (duration *duration*)
       (velocity *velocity*)
       (key *key*)
       (reference *reference*))
  "Play a sequence."
  (loop
     :with dur = (/ duration (length notes))
     :for tim :from time :by dur
     :for x :from 1
     :for accent = (if (= x 1)
		       1 *unaccent*)
     :for dura = (* dur accent)
     :for vel = (* velocity accent)
     :for note :across notes
     :do (setf
	  reference
	  (play note tim dura vel key reference)))
  reference)
