(define (factorial n)
  (if (eqv? n 0)
      1
      (* n (factorial (- n 1)))))

(factorial 5)
