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
