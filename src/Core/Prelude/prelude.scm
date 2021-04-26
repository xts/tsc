(define newline ()
  (display "\n"))

(define not (x)
  (if x #f #t))

(define > (x y)
  (< y x))

(define <= (x y)
  (not (< y x)))

(define >= (x y)
  (not (< x y)))

(define length (xs)
  (if (eq xs ())
      0
      (+ 1 (length (cdr xs)))))

(define reverse (xs)
  (define go (acc xs)
    (if (eq xs ())
        acc
        (go (cons (car xs) acc) (cdr xs))))
  (go () xs))
