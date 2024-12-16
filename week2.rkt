#lang simply-scheme

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (inc x) (+ x 1))

(define (identity n)
  n)

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

;; EXERCISE 1.31
(define (product term a next b)
  (if (> a b)
    1
    (* (term a) (product term (next a) next b))
  )
)

(define (factorial n)
  (product identity 1 inc n))

(define (pi-aprox n)
  (* 4.0 (/
    (* 2 (product (lambda (x) (* x x)) 4 (lambda (x) (+ x 2)) n))
    (product (lambda (x) (* x x)) 3 (lambda (x) (+ x 2)) n)
  ))
)

;; EXERCISE 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b)))
)

;; SUM DEFINED WITH ACCUMULATE
(define (accumulate-sum term a next b)
  (accumulate (lambda (x y) (+ x y)) 0 term a next b)
)

;; PRODUCT DEFINED WITH ACCUMULATE
(define (accumulate-product term a next b)
  (accumulate (lambda (x y) (* x y)) 1 term a next b)
)

;; EXERCISE 1.33
(define (filtered-accumulate combiner null-value term a next b predicate)
  (if (> a b)
    null-value
    (combiner (if (predicate a) (term a) null-value) (filtered-accumulate combiner null-value term (next a) next b predicate)))
)

;; SUM PRIMES
(define (sum-square-primes a b)
  (filtered-accumulate (lambda (x y) (+ x y)) 0 (lambda (x) (* x x)) a (lambda (x) (+ x 1)) b prime?)
)