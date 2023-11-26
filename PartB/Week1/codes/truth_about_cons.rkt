#lang racket
(provide (all-defined-out))

(define pr (cons 1 (cons #t "hi"))) ;wrong example? (1, (true,"hi"))
(define lst (cons 1 (cons #t (cons "hi" null))))