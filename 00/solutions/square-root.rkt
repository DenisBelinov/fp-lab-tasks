#lang racket

(provide my-sqrt)

(define (my-sqrt x)
  (define PRECISION 0.0001)
  
  (define (func y)
    (- (* y y) x))

  (define (func-der y)
    (* 2.0 y))

  (define (step s)
    (if (< (abs (- (* s s) x)) PRECISION)
        s
        (step (- s (/ (func s) (func-der s))))))
  
  (step 10))