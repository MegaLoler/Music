(defpackage :nyan-cat
  (:use :cl :music)
  (:export nyan-cat))
(in-package :nyan-cat)

(defsong (nyan-cat
	  :title "Nyan Cat"
	  :description "It's Nyan Cat."
	  :key (key 'b-minor)
	  :tempo (make-tempo 120)
	  :meter (make-meter 4 4))
  (flet ((intro () (seq '(> > > > 3 4 5 r 1 5 3 4 5 1 2 3 2 7 1 r
			  5 r 3 4 5 r 1 r 2 7 1 2 4 3 4 2 < < <)
			4))
	 (m1-1 () (seq '(> 5 r 6 r < ri 3 2 me 2 1 r 1 r 2 r
			 me r me 2 1 2 3 5 6 3 5 2 3 1 2 1)
		       '(4 4 4 4 4 2 4 4 4 4 4 4 4 4 4
			 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4)))
	 (m1-2 (c) (seq `(3 r 5 r 6 3 5 2 3 1 ri 3 me 2 1 2
			     me r 1 2 3 5 2 me 2 1 2 r 1 r ,@(if c '(1 r) '(2 r <)))
			4))
	 (m2-1 () (seq '(> 1 r 5 6 1 r 5 6 1 2 3 1 4 3 4 5
			 < 1 r 1 r 5 6 1 5 > 4 3 2 1 < 4 3 4 5)
		       4))
	 (m2-2 (c) (seq `(1 r 5 6 1 r 5 6 1 1 2 3 1 5 6 5
			  1 r 1 7 1 5 6 1 4 3 4 5 < 1 r ,@(if c '(2 r <) '(7 r <)))
			4))
	 (bass () (seq '(< < 4 r > 4 r < 5 r > 5 r < 3 r > 3 r < 6 r > 6 r
			 < < 2 r > 2 r < 5 r > 5 r < < 1 > 1 < 1 r 2 r 3 r > >)
		       4)))
    (let ((melody (cat (intro)
		       (m1-1) (m1-2 nil)
		       (m1-1) (m1-2 t)
		       (m2-1) (m2-2 nil)
		       (m2-1) (m2-2 t)))
	  (bass (cat (seq '(r) 1/8) (repeat (bass) 8))))
      (list bass melody))))

;; (play nyan-cat)
