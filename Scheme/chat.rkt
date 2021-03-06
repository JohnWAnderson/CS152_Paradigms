;;; CS 152 Homework 4 - A simple chatbot
;;; starter code
;;; Author John Anderson
;;; Version 1.1

;;; We'll use the random function implemented in Racket
;;; (random k) returns a random integer in the range 0 to k-1
(#%require (only racket/base random))

;;; some input and output helper functions

;;; prompt:  prompt the user for input
;;; return the input as a list of symbols
(define (prompt)
   (newline)
   (display "talk to me >>>")
   (read-line))

;;; read-line: read the user input till the eof character
;;; return the input as a list of symbols
(define (read-line)
  (let ((next (read)))
    (if (eof-object? next)
        '()
        (cons next (read-line)))))

;;; output: take a list such as '(how are you?) and display it
(define (output lst)
       (newline)
       (display (to-string lst))
       (newline))

;;; to-string: convert a list such as '(how are you?)
;;; to the string  "how are you?"
(define (to-string lst)       
  (cond ((null? lst) "")
        ((eq? (length lst) 1) (symbol->string (car lst)))
        (else (string-append (symbol->string (car lst))
                              " "
                             (to-string (cdr lst))))))


;;;  main function
;;;  usage:  (chat-with 'your-name)

(define (chat-with name)
  (output (list 'hi name))
  (chat-loop name))

;;; chat loop
(define (chat-loop name)
  (let ((input (prompt))) ; get the user input
    (if (eqv? (car input) 'bye)
        (begin
          (output (list 'bye name))
          (output (list 'have 'a 'great 'day!)))
        (begin
	  (reply input name)
          (chat-loop name)))))


;;; your task is to fill in the code for the reply function
;;; to implement rules 1 through 11 with the required priority
;;; each non-trivial rule must be implemented in a separate function
;;; define any helper functions you need below
;;;rule 1 test   (do|can|will|would) you

(define (reply input name)
     (cond((and (you-question-checker input)(pick-question input))(output(you-question-response input name '()))) ;rule 1 done need 6
     ((eqv? #t (special-checker input))(output(special-response input name '()))); rule 2 just end questionmark
     ((and(why-checker input)(pick-question input))(output(pick-random pick-why-question))) ;rule 3 done need 6
     ((and (how-checker input)(pick-question input))(output(pick-random pick-how-question))); rule 4 done nned 6
     ((and (what-checker input)(pick-question input))(output( pick-random pick-what-question))) ; rule 5 done need 6
     ((eqv? #t (pick-question input))(output (pick-random question-response))); rule 6 done
     ((eqv? #t (because-checker input))(output (pick-random pick-because-response)));rule 7 done
     ((eqv? #t (i-statment input))(output(i-response input))); rule 8 done
     ((eqv? #t (i-too input))(output(i-too-builder input))) ; rule 9 done
     ((eqv? #t (verb-checker input))(output(make-verb-repsonse input))) ; rule 10 done     
     (else(output (pick-random generic-response))))) ; rule 11 has been implemented for you done

;;; pick one random element from the list choices
(define (pick-random choices)
  (list-ref choices (random (length choices))))


;;; rule 1 test 
(define(you-question-checker input)
  (cond((null? input))
       ((eqv? 'do (car input))(you-question (cdr input)))
       ((eqv? 'can (car input))(you-question (cdr input)))
       ((eqv? 'will (car input))(you-question (cdr input)))
       ((eqv? 'would (car input))(you-question (cdr input)))))

(define(you-question input)
   (cond((eqv? 'you (car input))#t )
       (else #f)))


;;;rule 2 test _____(special topics)_______
(define(special-checker input)
  (cond((null? input)#f)
       ((eqv? 'family (car input))#t)
       ((eqv? 'friends (car input))#t )
       ((eqv? 'friend (car input))#t )
       ((eqv? 'mom (car input))#t )
       ((eqv? 'brother (car input))#t )
       ((eqv? 'sister (car input))#t )
       ((eqv? 'girlfriend (car input))#t )
       ((eqv? 'boyfriend (car input))#t )
       ((eqv? 'children (car input))#t )
       ((eqv? 'son (car input))#t )
       ((eqv? 'daughter (car input))#t )
       ((eqv? 'child (car input))#t )
       ((eqv? 'wife (car input))#t )
       ((eqv? 'husband (car input))#t )
       ((eqv? 'home (car input))#t )
       ((eqv? 'cat (car input))#t )
       ((eqv? 'dog (car input))#t )
       ((eqv? 'pet (car input))#t )
       (else (special-checker (cdr input)))))
       
;;;rule 3 test why __________?
(define(why-checker input)
  (cond((eqv? 'why (car input))#t )
       (else #f)))

;;;rule 4 test how __________?
(define(how-checker input)
  (cond((eqv? 'how (car input))#t )
       (else #f)))

;;; rule 5 test what __________?
(define(what-checker input)
    (cond((eqv? 'what (car input))#t )
       (else #f)))

;;;rule 6 test _____________?
(define(pick-question input)
  (cond((null? (cdr input))(string-last (string->list(symbol->string(car input)))(string-length (symbol->string (car input)))))
     (else(pick-question(cdr input)))))

(define (string-last input size)
  (cond((eqv? 1 size)(eqv? #\? (car input)))
  (else(string-last (cdr input)(- size 1)))))


;;;rule 7 test ______because_______
(define(because-checker input)
    (cond((null? input)#f)
       ((eqv? 'because (car input))#t )
       (else(because-checker (cdr input)))))

;;; rule 8 test i (need|think|have|want) ________

(define(i-statment input)
   (cond((eqv? 'i (car input))(i-question-checker (cdr input)))
       (else #f)))

(define(i-question-checker input)
  (cond((null? input)#f)
       ((eqv? 'need (car input))#t )
       ((eqv? 'think (car input))#t )
       ((eqv? 'have (car input))#t )
       ((eqv? 'want (car input))#t )
       (else #f)))

;;;rule 9 test i __________ (last word is not too)
(define(i-too input)
   (cond((eqv? 'i (car input))(i-too-checker (cdr input)))
       (else #f)))

(define(i-too-checker input)
  (cond((null? (cdr input))(eqv? #f(eqv? 'too (car input))))
       (else(i-too-checker(cdr input)))))

;;;rule 10 test verb ________________
(define(verb-checker input)
  (cond((null? input)#f)
       ((eqv? 'tell (car input))#t )
       ((eqv? 'say (car input))#t )
       ((eqv? 'give (car input))#t )
       ((eqv? 'move (car input))#t )
       (else #f)))

;;;rule 11 else is the implementaion



;;; generic responses for rule 11
(define generic-response '((that\'s nice)
                           (good to know)
                           (can you elaborate on that?)))
       
;;;responses for rule 1 done poor could have fit into less functions

(define(you-question-response input name set)
  (you-question-response-build (cddr input) name set (add (car input) '()) 2))

(define(you-question-response-build input name set word temp)
  (cond((eqv? 0 temp)(pick-random set))    
   ((eqv? temp 1)(you-question-response-build input name (add (you-question-response-first input name word) set) word (- temp 1)))
   (else(you-question-response-build input name (add (you-question-response-second word) set) word (- temp 1)))))

(define(you-question-response-first input name word)
  (cons 'no (you-question-response-first-second input name word)))

(define(you-question-response-first-second input name word)
  (cons name (you-question-response-first-third input word)))

(define(you-question-response-first-third input word)
  (cons 'i (you-question-response-first-forth input word)))

(define(you-question-response-first-forth input  word)
  (cons (car word) (you-question-response-first-not input)))

(define(you-question-response-first-not input)
  (cons 'not (you-question-response-first-fifth input)))

(define(you-question-response-first-fifth input)
  (cond((null? (cdr input))(remove-question-mark (car input) 1)) ;calls remove
       ((eqv? 'my (car input))(cons 'your(you-question-response-first-fifth (cdr input))))
       ((eqv? 'your (car input))(cons 'my(you-question-response-first-fifth (cdr input))))
       ((eqv? 'me (car input))(cons 'you (you-question-response-first-fifth (cdr input))))
       ((eqv? 'you (car input))(cons 'me (you-question-response-first-fifth (cdr input))))
       ((eqv? 'i (car input))(cons 'you(you-question-response-first-fifth (cdr input))))
       ((eqv? 'am (car input))(cons 'are(you-question-response-first-fifth (cdr input))))
       (else(cons(car input)(you-question-response-first-fifth (cdr input))))))

(define (remove-question-mark word number)
  (cond((eqv? 0 number)'())
  (else (cons (string->symbol(list->string(difference (string->list(symbol->string(checking-last-word-rule1 word))) (string->list "?")))) (remove-question-mark word 0))))); removes question mark

(define(checking-last-word-rule1 word)
    (cond((null? word)'()) ;calls remove
       ((eqv? 'my? word)'your)
       ((eqv? 'your? word)'my)
       ((eqv? 'me? word) 'you)
       ((eqv? 'you? word)'me)
       ((eqv? 'i? word)'you)
       ((eqv? 'am? word)'are)
       (else word)))

(define(you-question-response-second word)
  (cons 'yes (you-question-response-second-second word)))

(define(you-question-response-second-second word)
  (cons 'i (you-question-response-second-third word)))

(define(you-question-response-second-third word)
  (cons (car word)'()))




;;;responses for rule 2
(define(special-response input name set)
  (cond((null? input)(special-response-builder name set 7))
       ((eqv? 'family (car input))(special-response (cdr input) name (add (car input) set)))
       ((eqv? 'friends (car input))(special-response (cdr input) name (add (car input) set)))
       ((eqv? 'friend (car input))(special-response (cdr input) name (add (car input) set)))
       ((eqv? 'mom (car input))(special-response (cdr input) name (add (car input) set)))
       ((eqv? 'dad (car input))(special-response (cdr input) name (add (car input) set)))
       ((eqv? 'brother (car input))(special-response (cdr input) name (add (car input) set)))
       ((eqv? 'sister (car input))(special-response (cdr input) name (add (car input) set)))
       ((eqv? 'girlfriend (car input))(special-response (cdr input) name (add (car input) set)))
       ((eqv? 'boyfriend (car input))(special-response (cdr input) name (add (car input) set)))
       ((eqv? 'children (car input))(special-response (cdr input) name (add (car input) set)))
       ((eqv? 'son (car input))(special-response (cdr input) name (add (car input) set)))
       ((eqv? 'daughter (car input))(special-response (cdr input) name (add (car input) set)))
       ((eqv? 'child (car input))(special-response (cdr input) name (add (car input) set)))
       ((eqv? 'wife (car input))(special-response (cdr input) name (add (car input) set)))
       ((eqv? 'husband (car input))(special-response (cdr input) name (add (car input) set)))
       ((eqv? 'home (car input))(special-response (cdr input) name (add (car input) set)))
       ((eqv? 'cat (car input))(special-response (cdr input) name (add (car input) set)))
       ((eqv? 'dog (car input))(special-response (cdr input) name (add (car input) set)))
       ((eqv? 'pet (car input))(special-response (cdr input) name (add (car input) set)))
       (else (special-response (cdr input) name set))))

(define(special-response-builder name set number)
 (cond((eqv? number 0)'())
      ((eqv? number 7)(cons 'tell (special-response-builder name set (- number 1))))
      ((eqv? number 6)(cons 'me (special-response-builder name set (- number 1))))
      ((eqv? number 5)(cons 'more (special-response-builder name set (- number 1))))
      ((eqv? number 4)(cons 'about (special-response-builder name set (- number 1))))
      ((eqv? number 3)(cons 'your (special-response-builder name set (- number 1))))
      ((eqv? number 2)(cons (pick-random set)(special-response-builder name set (- number 1))))
      (else(cons name (special-response-builder name set (- number 1))))))



;;; responses for rule 3
(define pick-why-question '((why not?)))

;;; responses for rule 4
(define pick-how-question '((why do you ask?)
                           (how would an answer to that help you?)))

;;;responses for rule 5
(define pick-what-question '((why do you ask?)
                           (what do you think?)))

;;; responses for rule 6
(define question-response '((i don\'t know)
                           (i have no clue)
                           (i have no idea)
                           (maybe)))
;;; responses for rule 7
(define pick-because-response '((is that the real reason?)))

;;;responses for rule 8
(define (i-response input)
  (append '(why do you)(i-response-second (cdr input))))

(define (i-response-second input)
  (cons (car input)(i-response-builder (cdr input))))

(define (i-response-builder input)
  (cond((null? (cdr input)) (cons (adds-question-mark (string->list(symbol->string(checking-last-word(car input)))))'())) ; adds question mark
       ((eqv? 'my (car input))(cons 'your(i-response-builder (cdr input))))
       ((eqv? 'your (car input))(cons 'my(i-response-builder (cdr input))))
       ((eqv? 'me (car input))(cons 'you (i-response-builder (cdr input))))
       ((eqv? 'you (car input))(cons 'me (i-response-builder (cdr input))))
       ((eqv? 'i (car input))(cons 'you(i-response-builder (cdr input))))
       ((eqv? 'am (car input))(cons 'are(i-response-builder (cdr input))))
       (else(cons(car input)(i-response-builder (cdr input))))))

(define (adds-question-mark input)
  (string->symbol(list->string(union input (string->list "?")))))

(define(checking-last-word word)
    (cond((null? word)'()) ;calls remove
       ((eqv? 'my word)'your)
       ((eqv? 'your word)'my)
       ((eqv? 'me word) 'you)
       ((eqv? 'you word)'me)
       ((eqv? 'i word)'you)
       ((eqv? 'am word)'are)
       (else word)))


;;; responses for rule 9
(define(i-too-builder input)
  (cond((null? input)'(too))
   (else(cons (car input)(i-too-builder (cdr input))))))

;;; presponses for rule 10
(define(make-verb-repsonse input)
  (cons 'you (make-verb-sentence input)))

(define(make-verb-sentence input)
  (cond((null? input)'())
   (else(cons (car input)(make-verb-sentence (cdr input))))))


;;; generic responses for rule 11
(define generic-response '((that\'s nice)
                           (good to know)
                           (can you elaborate on that?)))

;;;; hw 3 code below put in if uses
(define (empty? s)
  (null? s)
)

(define(set s)(cond ((null? s) '())
     ((in? (car s)(cdr s))(set(cdr s)))
     (else(cons (car s)(set(cdr s))))))

(define (in? e s)
  (cond((null? s) #f)
       ((eqv? e (car s))#t )
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
    (else #f)))

          
(define (superset? s1 s2)(cond((and (null? s1)(null? s2))lst)
    ((null? s2)(null? s2))
    ((in? (car s2) s1)(superset? s1 (cdr s2)))
    (else #f)))


(define (disjoint? s1 s2)(cond((and(null? s1)(null? s2))#f)
        ((null? s1)#t )
        ((in? (car s1) s2)#f)
        (else(disjoint? (cdr s1) s2))))

(define(sameset? s1 s2)(cond((and(null? s1)(null? s2))#t )
        ((null? s1)#t )
        ((in? (car s1)s2)(sameset?(cdr s1)s2))
        (else #f))) 




