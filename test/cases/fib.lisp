(define fib (k)
  (if (< k 3)
    1
    (+ (fib (- k 1))
       (fib (- k 2)))))

(display (fib 30))
(display "\n")
