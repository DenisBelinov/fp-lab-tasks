#lang racket

(provide from-numeral
         to-numeral
         plus
         mult
         pred)

(define zero (lambda (f v) v))
(define one (lambda (f v) (f v)))
(define two (lambda (f v) (f (f v))))

(define (1+ n) (+ 1 n))

(define (succ n)
  (lambda (f v)
    (f (n f v))))

(define (from-numeral n) (n 1+ 0))

(define (to-numeral n)
  (if (= n 0)
      zero
      (succ (to-numeral (- n 1)))))

(define (plus n m)
  (n succ m))


(define (mult n m)
  (n (lambda (numeral) (plus m numeral))
     zero))

(define (pred n)
  (lambda (f v) ((n
                  (lambda (numeral)
                    (if (equal? ((succ numeral) 1+ 0) (n 1+ 0))
                        numeral
                        (succ numeral)))
                  zero)
                 f v)))
