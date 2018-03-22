# Music

Just a framework for musical expression in Common Lisp with a focus on music theory!

Totally building it from scratch, there's lots to do and a lot is missing/broken.

It currently uses SBCL timers for queuing real time midi events, so it's SBCL dependant.

See `examples/nyan.lisp` for an example of a song.

## Examples

You can designate pitch classes with symbols:

`'c` `'c♯` `'f♭` `'g♯♯♯`

And actual notes with octaves as well:

`'c5` `'a♭♭7`

You can designate intervals with symbols:

`'p5` `'maj7` `2dim2` (doubly diminished second)

You can designate harmonies like this:

`'ii` `'IVdom9` `'vmin42♯5`

You can make a harmony like this:

```lisp
(harmony 'I7/V)
;; => (D F♯ A C)

(harmony 'IVmaj43)
;; => (C E F A)
```

You can get a scale like this:

```lisp
(scale (key 'g-phrygian))
;; => (G A♭ B♭ C D E♭ F)
```

You can designate scale degrees with either numbers, names or solfège syllables:

`'(do re 3 4 dominant submediant)`

You can realize scale degrees and pitch class like this:

```lisp
(realize '(do re 3 4 dominant submediant))
;; => (C4 E4 G4 F4 B4 A4 B4 G2)
```

You can also nest them:

```lisp
(realize '(do re mi (fa sol la) ti do))
;;; => (C4 D4 E4 (F4 G4 A4) B4 C5)
```

And indicate octave displacements and rests like this:

```lisp
(realize '(mi fa > mi fa > mi r < < < < re r))
;; => (E4 F4 E5 F5 E6 R D2 R)
```

You can realize harmonic progressions in a simple way like this:

```lisp
(realize (harmony '(I IV64 V43 I6)))
;; => ((C4 E4 G4) (C4 F4 A4) (D4 F4 G4 B4) (E4 G4 C5))
```

You can get the interval between notes like this:

```lisp
(difference (note 'g5) (note 'f6))
;; => MIN7
```

You can get the note or pitch class an interval above or below another like this:

```lisp
(above (note 'a♭4) (interval 'p8))
;; => A♭5
```

```lisp
(below (pitch-class 'd) (interval 'aug2))
;; => C♭
```

You can add intervals like this:

```lisp
(sum (interval 'maj6) (interval 'maj3)
;; => AUG8
```

Why not stack 100 perfect fifths together and see what you get?

```lisp
(loop
   :with i = (interval 'p1)
   :repeat 100
   :do (setf i (sum i (interval 'p5)))
   :finally (return i))
;; => 14AUG401
```

You can get relative or parallel keys like this:

```lisp
(parallel (key 'f-major) 'minor)
;; => F-MINOR

(relative (key 'f♯-minor) 'mixolydian)
;; => E-MIXOLYDIAN
```

You can make a sequence like this:

```lisp
(seq '(> sol la < ri mi re me re do do re))
```

You can make a chord like this:

```lisp
(chord '(do mi sol te))
```

You can designate rhythms using divisions of beats like this:

```lisp
;; "one and two and three and four
(2 2 2 2 2 2 1 2 2 2 2 2 2 1) ;; twinkle twinkle

;; "one-ee uh ee-and three four"
(4 2 2 4 2 1 1) ;; mario theme opening rhythm
```

And you can give rhythm to a sequence like this:

```lisp
(seq '(> sol la < ri mi re me re do do re)
     '(2 2 4 2 4 4 4 2 2 2))
```

You can play things with live midi using portmidi like this:

```lisp
(play (seq '(> sol la < ri mi re me re do do re)
	       '(2 2 4 2 4 4 4 2 2 2)))
```

Which implicitely wraps the expression in `event` first:

```lisp
(event (seq '(> sol la < ri mi re me re do do re)
	        '(2 2 4 2 4 4 4 2 2 2)))
;; => (G4@0s-1/4s A4@1/4s-1/2s D♯4@1/2s-5/8s E4@5/8s-7/8s D4@7/8s-1s E♭4@1s-9/8s D4@9/8s-5/4s C4@5/4s-3/2s C4@3/2s-7/4s D4@7/4s-2s)
```

Which turns the expression into a list of timed events.


