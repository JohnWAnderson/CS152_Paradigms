(define (empty? s)
  (null? s)
)

(define(set s)(cond ((null? s) '())
     ((in? (car s)(cdr s))(set(cdr s)))
     (else(cons (car s)(set(cdr s))))))

(define (in? e s)
  (cond((null? s) (eqv? 1 0))
       ((eqv? e (car s))(eqv? 1 1))
       (else(in? e (cdr s))))
)

(define(add e s)(cond((in? e s) s)
          (else(cons e s))))

(define(discard e s)(cond ((null? s) '())
     ((eqv? e (car s))(cdr s))
     (else(cons (car s)(discard e (cdr s))))))

(define(tunion s1 s2 lst)(cond ((and (null? s1)(null? s2))lst)
       ((null? s1)(tunion s1 (cdr s2)(append lst (list(car s2)))))
       ((null? s2)(tunion (cdr s1) s2 (append lst (list(car s1)))))
       ((in? (car s1) s2)(tunion(cdr s1) s2 lst))
       (else(tunion(cdr s1) s2 (append lst (list(car s1)))))))

(define(union s1 s2)(tunion s1 s2 '()))


(define(tintersection s1 s2 lst)(cond ((and (null? s1)(null? s2))lst)
     ((null? s1)lst)
     ((in? (car s1) s2)(tintersection(cdr s1) s2 (append lst (list(car s1)))))	
     (else(tintersection(cdr s1) s2 lst))))

(define(intersection s1 s2)(tintersection s1 s2 '()))


(define(tdifference s1 s2 lst)(cond ((and (null? s1)(null? s2))lst)
      ((null? s1)lst)
      ((in? (car s1) s2)(tdifference(cdr s1) s2 lst))	
      (else(tdifference(cdr s1) s2 (append lst (list(car s1)))))))

(define(difference s1 s2)(tdifference s1 s2 '()))


(define(tsymmetric-difference s1 s2 lst)(cond ((and (null? s1)(null? s2))lst)
       ((null? s1)(tsymmetric-difference s1 (cdr s2)(append lst (list(car s2)))))
       ((null? s2)(tsymmetric-difference (cdr s1) s2 (append lst (list(car s1)))))
       ((in? (car s1) s2)(tsymmetric-difference(cdr s1) (discard (car s1) s2) lst))
       (else(tsymmetric-difference(cdr s1) s2 (append lst (list(car s1)))))))

(define(symmetric-difference s1 s2)(tsymmetric-difference s1 s2 '()))

(define (subset? s1 s2)(cond((and (null? s1)(null? s2))lst)
    ((null? s1)(null? s1))
    ((in? (car s1) s2)(subset? (cdr s1) s2))
    (else(eqv? 1 0))))

          
(define (superset? s1 s2)(cond((and (null? s1)(null? s2))lst)
    ((null? s2)(null? s2))
    ((in? (car s2) s1)(superset? s1 (cdr s2)))
    (else(eqv? 1 0))))


(define (disjoint? s1 s2)(cond((and(null? s1)(null? s2))(eqv? 1 0))
        ((null? s1)(eqv? 1 1))
        ((in? (car s1) s2)(eqv? 1 0))
        (else(disjoint? (cdr s1) s2))))

(define(sameset? s1 s2)(cond((and(null? s1)(null? s2))(eqv? 1 1))
        ((null? s1)(eqv? 1 1))
        ((in? (car s1)s2)(sameset?(cdr s1)s2))
        (else(eqv? 1 0)))) 

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