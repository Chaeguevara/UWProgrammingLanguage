
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence x y z)
  (if (> x y)
   null 
  (cons x (sequence (+ x z) y z))))

(define (string-append-map lst)
  (map (lambda (x)
         (string-append x ".jpg")
         )
       lst))

(string-append-map (list "dan" "dog" "curry" "dog2"))

(define (list-nth-mod lst n)
  (cond[(< n 0) (error "list-nth-mod: negative number")]
       [(null? lst) (error "list-nth-mod: empty list")]
       [#t (if (equal? 0 (remainder n(length lst)))
             (car lst)
             (list-nth-mod (cdr lst) (- n 1)))]
    ))

(list-nth-mod (list 0 1 2 3 4) 2)

(define (stream-for-n-steps s n)
  (letrec ([f (lambda (s cnt)
                (let ([pr (s)])
                  (if (= cnt n)
                    '()
                    (cons (car pr) (f  (cdr pr) (+ cnt 1)  ) )
                    )))])
   (f s 0)))

(define ones (lambda () (cons 1 ones)))

(stream-for-n-steps ones 2)

(define funny-number-stream 
  (letrec([f (lambda (x)
             (let ([rem (remainder x 5)])
             (if (equal? rem 0)
               (cons (* x -1) (lambda()(f(+ x 1))))
               (cons x (lambda()(f(+ x 1))))
               )))])
    (lambda()(f 1)))
  )

(car((cdr(funny-number-stream))))
(stream-for-n-steps funny-number-stream 16) 

(define dan-then-dog
  (letrec([f (lambda (x)
               (let ([is_even (even? x)])
                (if (equal? is_even #t)
                  (cons "dog.jpg" (lambda() (f(+ x 1))))
                  (cons "dan.jpg" (lambda() (f(+ x 1))))
                  )))])
  (lambda()(f 1)))
 ) 


(stream-for-n-steps dan-then-dog 2) 

(define (stream-add-zero s)
  (letrec ([f (lambda(x)
                (let ([pr (x)])
                  (cons (cons 0 (car pr)) (lambda()(f (cdr pr))))
                  )) ])
    (lambda()(f s)))
  )

(define (cycle-lists xs ys)
  (letrec ([f (lambda(xidx yidx)
                (let ([x_len  (length xs) ]
                      [y_len  (length ys) ]
                      )
                  (cond [(and (< xidx x_len) (< yidx y_len))(cons (cons (list-ref xs xidx)(list-ref ys yidx ) ) (lambda()(f (+ xidx 1) (+ yidx 1))) ) ]  
                        [(< xidx x_len) (cons (cons (list-ref xs xidx)(list-ref ys 0 ) ) (lambda()(f (+ xidx 1) 1)) )]
                        [(< yidx y_len) (cons (cons (list-ref xs 0)(list-ref ys yidx ) ) (lambda()(f 1 (+ yidx 1))) )]
                        [#t (cons (cons (list-ref xs 0)(list-ref ys 0 ) ) (lambda()(f 1 1)) )]
                        )
                  )
              ) ]
           )
  (lambda()(f 0 0)))

  )


(dan-then-dog)
(stream-add-zero ones)
(stream-for-n-steps (stream-add-zero ones) 2)
(stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 4)
