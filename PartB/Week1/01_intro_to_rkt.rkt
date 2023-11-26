#lang racket 

(provide (all-defined-out))

(define (cube3 x)
  (* x x x))

(display "(cube3 3):")
(cube3 3) ;; print 27

(define (pow x y)
  (if (= y 0)
    1
    (* x (pow x (- y 1)))))
  
(display "(pow 3 2):")
 (pow 3 2) ;; 9

(define pow2
    (lambda (x) 
      (lambda (y)
        (if (= y 0)
          1
          (* x ((pow2 x) (- y 1)))))))

(define ((pow3 x) y)
  (if (= y 0)
    1
    (* x ((pow3 x) (- y 1)))))


(define three-to-the (pow2 3)) ;;partial application
(define eighty-one (three-to-the 4))
(define six-teen ((pow2 2) 4))
(define new-six-teen ((pow3 4) 2))


(display "(three-to-the 3):")
(three-to-the 3) ;; 27

(display "eighty-one: ")
eighty-one ;; 81

(display "six-teen: ")
six-teen ;; 16

(display "((pow2 2)3):")
((pow2 2) 3) ;; 9

(display "new-six-teen:")
new-six-teen ;;16



(define list1 (cons 3(cons 4(cons 5 null))))
(define (sum xs)
    (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))
(display "list1:")
list1
(display "sum list1: ")
(sum list1)

(define (map f xs)
  (if (null? xs)
    null
    (cons (f (car xs)) (map f (cdr xs)))))
(display "map (+) list1:")
(map three-to-the list1)
