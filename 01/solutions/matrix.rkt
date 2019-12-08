#lang racket

(provide all?
         any?
         concat
         rows
         cols
         matrix-ref
         set
         place
         diag
         diags
         map-matrix
         filter-matrix
         zip-with
         zip-matrix)
; the provide "exports" these functions

; 00.
(define (all? p? xs)
  (if (null? xs)
      true
      (and (p? (car xs)) (all? p? (cdr xs)))))

; 01.
(define (any? p? xs)
    (if (null? xs)
      false
      (or (p? (car xs)) (any? p? (cdr xs)))))

; 02.
(define (concat xss)
  (if (null? xss)
      '()
      (if (null? (car xss))
          (concat (cdr xss))
          (cons (caar xss) (concat (append (list (cdar xss)) (cdr xss)))))))

; 03.
(define (rows xss) xss)

; 04.
(define (cols xss)
  (if (null? (car xss))
      '()
      (cons (map car xss) (cols (map cdr xss)))))

; 05.
(define (matrix-ref xss i j)
  (if (= i 0)
      (list-ref (car xss) j)
      (matrix-ref (cdr xss) (- i 1) j)))

; 06.
(define (set xs i x)
  (if (null? xs)
      '()
      (if (= i 0)
          (cons x (cdr xs))
          (cons (car xs) (set (cdr xs) (- i 1) x)))))

; 07.
(define (place xss i j x)
  (if (= i 0)
      (cons (set (car xss) j x) (cdr xss))
      (cons (car xss) (place (cdr xss) (- i 1) j x))))

; 08.
(define (diag xss)
  (if (null? xss)
      '()
      (cons (caar xss) (diag (map cdr (cdr xss))))))

; 09.
(define (flip xss)
  (map reverse xss))

(define (diags xss)
  (cons (diag xss) (list (diag (flip xss)))))

; 10.
(define (map-matrix f xss)
  (if (null? xss)
      '()
      (cons (map f (car xss)) (map-matrix f (cdr xss)))))

; 11.
(define (filter-matrix p? xss)
    (if (null? xss)
      '()
      (cons (filter p? (car xss)) (filter-matrix p? (cdr xss)))))

; 12.
(define (zip-with f xs ys)
  (if (or (null? xs) (null? ys))
      '()
      (cons (f (car xs) (car ys)) (zip-with f (cdr xs) (cdr ys)))))

; 13.
(define (zip-matrix xss yss)
  (if (or (null? xss) (null? yss))
      '()
      (cons (zip-with cons (car xss) (car yss)) (zip-matrix (cdr xss) (cdr yss)))))
