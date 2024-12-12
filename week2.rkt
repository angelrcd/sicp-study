#lang simply-scheme

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (inc x) (+ x 1))


;; EXERCISE 1.29
(define (simpson f a b n)
  (define h 
    (/ (- b a) n)
  )
  (define (term k)
    (* (multiply-term k) (f (+ a (* k h))))
  )
  (define (multiply-term k)
    (cond ((or (= k 0) (= k n)) 1)
      ((= (remainder k 2) 0) 2)
      (else 4)))
  (* (/ h 3) (sum term 0.0 inc n))
)