(define (newline)
  (display "\n"))

(define (not x)
  (if x #f #t))

(define (> x y)
  (< y x))

(define (<= x y)
  (not (< y x)))

(define (>= x y)
  (not (< x y)))

;; Find the length of the list XS.
(define (length xs)
  (if (eq xs ())
      0
      (+ 1 (length (cdr xs)))))

;; Reverse list XS.
(define (reverse xs)
  (define (go acc xs)
    (if (eq xs ())
        acc
        (go (cons (car xs) acc) (cdr xs))))
  (go () xs))

;; Sort a numeric list XS with insertion sort.
(define (sort xs)
  (define (insert x xs)
    (if (eq xs ())
      (list x)
      (if (< (car xs) x)
          (cons (car xs) (insert x (cdr xs)))
          (cons x xs))))

  (define (go acc xs)
    (if (eq xs ())
        acc
        (go (insert (car xs) acc) (cdr xs))))

  (go () xs))

;; Map F across all elements of XS.
(define (mapcar f xs)
  (if (eq xs ())
      ()
      (cons (f (car xs)) (mapcar f (cdr xs)))))
