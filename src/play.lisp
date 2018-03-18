(in-package :music)

;; this is just for quick testing??
;; i'll make a better performance system later

(deftype musical-rest ()
  "Symbols representing a musical rest."
  `(member rest r))

(defvar *duration* 1)
(defvar *velocity* 80)
(defvar *unaccent* 4/5)
(defvar *reference* (note 'c4))
(defvar *key* (key 'c-major))

(defvar *midi-out* nil)

(defun init-midi (&optional (out-id (pm:get-default-output-device-id)))
  "Initialize midi output with port midi."
  (if *midi-out*
      (error "A midi device is already open!")
      (setf *midi-out* (pm:open-output out-id 1024 0))))

(defun close-midi ()
  "Close the open midi connection with port midi."
  (if *midi-out*
      (setf *midi-out* (pm:close-midi *midi-out*))
      (error "A midi device is not open yet!")))

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
  (if *midi-out*
      (progn
	(format t "Playing note ~A at time ~A for ~As with velocity ~A.~%"
		(chromatic-value note) time duration velocity)
	(schedule-timer
	 (make-timer
	  (lambda ()
	    (pm:write-short-midi *midi-out* 0
				 (pm:note-on 0
					     (chromatic-value note)
					     (floor velocity))))
	  :thread t)
	 time)
	(schedule-timer
	 (make-timer
	  (lambda ()
	    (pm:write-short-midi *midi-out* 0
				 (pm:note-off 0
					     (chromatic-value note)
					     (floor velocity))))
	  :thread t)
	 (+ time duration)))
      (error "Open a midi device first!"))
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
  (flet ((special (a) (find-if (lambda (b)
			   (eql a b))
			 '(< >))))
  (loop
     :with len = (count-if (lambda (note)
			     (not (special note)))
			   notes)
     :with dur = (/ duration len)
     :for tim :from time :by dur
     :for x :from 1
     :for accent = (if (= x 1)
		       1 *unaccent*)
     :for vel = (* velocity accent)
     :for note :across notes
     :do (if (special note)
	     (progn
	       (setf tim (- tim dur))
	       (case note
		 (> (setf reference
			  (above reference (interval 'p8))))
		 (< (setf reference
			  (below reference (interval 'p8))))))
	     (setf
	      reference
	      (play note tim dur vel key reference))))
  reference))
