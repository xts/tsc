(define input (list 42 123 -4 1222 11 2 33 128))

(define insert (x xs)
  (if (eq xs ())
    (list x)
    (if (< (car xs) x)
        (cons (car xs) (insert x (cdr xs)))
        (cons x xs))))

(define sort (xs)
  (define go (acc xs)
    (if (eq xs ())
        acc
        (go (insert (car xs) acc) (cdr xs))))
  (go () xs))

(display input)
(newline)
(display "=> ")
(display (sort input))
(newline)
