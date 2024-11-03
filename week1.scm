#lang simply-scheme

; EXERCISE 2
(define (squares list)
  (cond
    ((empty? list) '())
    (else (sentence (square (first list)) (squares (bf list))))
  )
   
)

(define (square x) (* x x))

; EXERCISE 3
(define (switch input)
  (sentence (switch-first-word (first input)) (switch-iter (bf input)))
)

(define (switch-iter input)
     (if (empty? input)
      '()
      (sentence (switch-word (first input)) (switch-iter (bf input))
  )
))

(define (switch-word word)
   (cond
      ((or (equal? word 'I) (equal? word 'me)) 'you)
      ((equal? word 'you) 'me)
      (else word)
   )
)

(define (switch-first-word word)
    (cond
      ((or (equal? word 'I) (equal? word 'Me)) 'You)
      ((equal? word 'You) 'I)
      (else word)
   )
)

; EXERCISE 4
(define (ordered? numbers)
  (if (empty? (bf numbers))
      #t
      (and (<= (first numbers) (first (bf numbers))) (ordered? (bf numbers)))
  )
)

;EXERCISE 5
(define (ends-e words)
  (if (empty? words)
      '()
      (sentence (self-if-ends-e (first words)) (ends-e (bf words))))
  )


; Return the word if it ends with e, empty if not
(define (self-if-ends-e word)
    (if (equal? (last word) 'e)
        word
        '()
    )
)