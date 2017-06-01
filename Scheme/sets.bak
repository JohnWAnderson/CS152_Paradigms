(define (empty? s)
  'enter-your-code-here
)

(define (set s)
  'enter-your-code-here
)

(define (in? e s)
  'enter-your-code-here
)

(define (add e s)
  'enter-your-code-here
)

(define (discard e s)
  'enter-your-code-here
)       

(define (union s1 s2)
  'enter-your-code-here
)

(define (intersection s1 s2)
  'enter-your-code-here
)

(define (difference s1 s2)
  'enter-your-code-here
)

(define (symmetric-difference s1 s2)
  'enter-your-code-here
)

(define (subset? s1 s2)
  'enter-your-code-here
)
          
(define (superset? s1 s2)
  'enter-your-code-here
)

(define (disjoint? s1 s2)
  'enter-your-code-here
)

(define (sameset? s1 s2)
  'enter-your-code-here
)


; some tests
(define A (set '(1 2 7 9 7 1)))
(define B (set '(2 0 8 0 7 12)))
(define C (set '(9 7)))

(define colors (set '("yellow" "red" "green" "blue" "orange" "purple" "pink")))
(define rgb (set '("red" "green" "blue")))

(define hi (set '(#\h #\i)))

(empty? A) ; #f
(empty? rgb) ;#f
(empty? (set'())) ;#t

(in? 0 A) ; #f
(in? "red" A); #f
(in? 2 A) ; #t

(in? "green" rgb) ; #t
(in? "purple" rgb) ; #f
(in? "i" hi) ;#f
(in? #\i hi) ;#t

(add 9 A) ; (2 9 7 1)
(add 5 A) ; (5 2 9 7 1)

(discard 1 A) ; (2 9 7)
(discard 5 A) ; (2 9 7 1)
(union A B) ; (9 1 2 8 0 7 12)
(union A rgb) ; (2 9 7 1 "red" "green" "blue")

(intersection A rgb) ; ()
(intersection A B) ; (2 7)
(intersection rgb colors) ; ("red" "green" "blue")

(difference A B) ; (9 1)
(difference rgb colors) ; ()
(difference colors rgb) ; ("yellow" "orange" "purple" "pink")

(symmetric-difference A B) ; (9 1 8 0 12)
(symmetric-difference A C) ; (2 1)
(symmetric-difference colors rgb) ; ("yellow" "orange" "purple" "pink")

(subset? A B) ;#f
(subset? C A) ; #t

(subset? colors rgb) ;#f
(subset? rgb colors)  ; #t

(superset? A B) ;#f
(superset?  A C) ; #t

(superset? colors rgb) ;#t
(superset? rgb colors)  ; #f

(disjoint? B C) ;#f
(disjoint? colors A) ;#t

(sameset? (set '(9 1 2 7)) A); #t
(sameset? B A) ; #f