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

(define (null? xs)
  (eq xs ()))

;; Map F across all elements of XS.
(define (map f xs)
  (if (null? xs)
      ()
      (cons (f (car xs)) (map f (cdr xs)))))

;; Fold right.
(define (fold-right f init xs)
   (if (null? xs)
       init
       (f (car xs)
          (fold-right f init (cdr xs)))))

;; Fold left.
(define (fold-left f init xs)
   (if (null? xs)
       init
       (fold-left f (f (car xs) init) (cdr xs))))

;; Find the length of the list XS.
(define (length xs)
  (fold-right (lambda (x acc) (+ acc 1)) 0 xs))

;; Reverse list XS.
(define (reverse xs)
  (fold-left cons () xs))

;; Sort a numeric list XS with insertion sort.
(define (sort xs)
  (define (insert x xs)
    (if (null? xs)
      (list x)
      (if (< (car xs) x)
          (cons (car xs) (insert x (cdr xs)))
          (cons x xs))))

  (fold-right insert () xs))
