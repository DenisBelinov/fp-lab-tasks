#lang racket

(provide winner
         play)

(require "matrix.rkt")
; You can use your matrix functions below, thanks to the "require" invocation above.

(define (id x) x)

; winner implementation that only detects draws right now.
; Put your own implementation here!

(define (get-winner xs)
  (andmap (lambda (x)
            (and (equal? x (car xs)) x))
          xs))

(define (horizontal-winner b)
  (ormap get-winner b))

(define (vertical-winner b)
  (ormap get-winner (cols b)))

(define (diag-winner b)
  (ormap get-winner (diags b)))


(define (winner b)
  (define winner (or (horizontal-winner b) (vertical-winner b) (diag-winner b)))
  (or 
   winner
   (if (andmap (lambda (xs) (andmap id xs)) b)
       "D"
       #f)))


; "Dumb" "AI", plays the "next" free spot, going left-to-right, top-to-bottom.
; Put your own implementation here!

; returns a list of the indexes (ex. (0, 2)) which are false
(define (get-free-places b)

  (define (helper b i j)
    
    (define BOARD-SIZE (length b))
    
    (cond ((>= j BOARD-SIZE) (helper b (+ 1 i) 0))
          ((>= i BOARD-SIZE) '())
          (else (let* ((curr-el (matrix-ref b i j))
                       (rec (helper b i (+ 1 j))))
                  (if curr-el
                      rec
                      (cons (list i j) rec))))))

  (helper b 0 0))

(define (get-possible-future-boards b sign)
  (map (lambda (p)
         (place b (car p) (cadr p) sign))
       (get-free-places b)))

; returns assoc list with (board i j) where i and j are the coords of where the given sign was placed, used in play function
(define (get-board-free-place-assoc b sign)
  (define places (get-free-places b))
  (define boards (get-possible-future-boards b sign))

  (map cons boards places))


; minimax algs
(define (max-alg b)
  (define w (winner b))
  
  (cond ((equal? w "X") 1)
        ((equal? w "O") -1)
        ((equal? w "D") 0)
        (else (let ((possible-future-boards (get-possible-future-boards b "X")))
                (apply max (map min-alg possible-future-boards))))))

(define (min-alg b)
  (define w (winner b))
  
  (cond ((equal? w "X") 1)
        ((equal? w "O") -1)
        ((equal? w "D") 0)
        (else (let ((possible-future-boards (get-possible-future-boards b "O")))
                (apply min (map max-alg possible-future-boards))))))


; I know where I use these, no need for null checks trust me :)
(define (max-first-elems xs1 xs2)
  (if (>= (car xs1) (car xs2))
      xs1
      xs2))

(define (min-first-elems xs1 xs2)
  (if (<= (car xs1) (car xs2))
      xs1
      xs2))


; these funcs are meant to be used on a board->free-place assoc array's element
(define (apply-max-alg xs)
  (cons (max-alg (car xs)) (cdr xs)))

(define (apply-min-alg xs)
  (cons (min-alg (car xs)) (cdr xs)))


; entrypoint funcs
(define (maximise curr-board)
  (let* ((actions (get-board-free-place-assoc curr-board "X"))
         (move (cdr (foldr max-first-elems '(-10 100 100)
                           (map apply-min-alg actions)))))
    (cons (car move) (cadr move))))

(define (minimize curr-board)
  (let* ((actions (get-board-free-place-assoc curr-board "O"))
         (move (cdr (foldr min-first-elems '(10 100 100)
                     (map apply-max-alg actions)))))
    (cons (car move) (cadr move))))

(define (play curr-board curr-sign)
  (if curr-sign
      (maximise curr-board)
      (minimize curr-board)))