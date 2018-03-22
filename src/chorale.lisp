(in-package :music)

;; an experimental chorale generator
;; better idea: use nondeterminism and constraints

(defun bad-parallel-p (a1 b1 a2 b2)
  "Whether two notes are an illegal parallel octave or fifth."
  (let ((a1 (note a1))
	(a2 (note a2))
	(b1 (note b1))
	(b2 (note b2)))
    (and a1 b1 a2 b2
	 (let ((i (difference a1 b1))
	       (i2 (difference a2 b2)))
	   (when (or (interval-diatonic-equal i 'p5)
		     (interval-diatonic-equal i 'p8)
		     (interval-diatonic-equal i 'p1))
	     (and (interval-diatonic-equal i i2)
		  (not (note-equal a1 a2))))))))

(defun make-voice (progression &optional voices previous last (env (default-environment)))
  "Part-write a voice following a chord progression."
  (when progression
    (let* ((candidates (apply #'append (mapcar #'realize-range (car progression))))
	   (ref (reference env))
	   (current (mapcar #'car voices))
	   (motions (mapcar #'cons current previous))
	   (bass (car (last current)))
	   (sorted (score-sort candidates
			       (lambda (note)
				 (let ((my-motion (cons note last)))
				   (not (any-p t (mapcar (lambda (motion)
							   (bad-parallel-p (car my-motion)
									   (car motion)
									   (cdr my-motion)
									   (cdr motion)))
							 motions)))))
			       (lambda (note)
				 (or (not last)
				     (<= (diatonic-value (difference note last))
					 5)))
			       ;; no crossing voices
			       (lambda (note)
				 (<= (diatonic-value (difference note bass))
				     25))
			       (lambda (note)
				 (- (count-if (lambda (a)
						(note-equal a note))
					      current)))
			       (lambda (note)
				 (- (count-if (lambda (a)
						(pitch-class-equal (pitch-class a)
								   (pitch-class note)))
					      current)))
	   		       (lambda (note)
	   			 (- (diatonic-value (difference ref note))))))
	   (choice (car sorted)))
      (cons choice
	    (make-voice (cdr progression)
			(mapcar #'cdr voices)
			current
			choice
			(ref-env choice env))))))

(defun make-bass-voice (progression &optional (env (default-environment)))
  "Part-write a bass voice following a chord progression."
  (realize (mapcar #'car progression) env))

(defun make-chorale (progression &optional (voice-count 3) voices (env (default-environment)))
  "Part-write a chorale following a chord progression."
  (let ((voices (or voices (list (make-bass-voice progression (ref-env 'c2))))))
    (if (> voice-count 0)
	(let ((voices (append (list (make-voice progression voices nil nil env)) voices)))
	  (make-chorale progression (1- voice-count) voices env))
	voices)))
