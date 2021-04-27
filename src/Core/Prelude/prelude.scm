()

;; Wrap fixed-arity primitives to make them first-class.
(define (display x)
  (#display x))

(define (eq x y)
  (#eq x y))

(define (= x y)
  (#= x y))

(define (< x y)
  (#< x y))

(define (cons a b)
  (#cons a b))

(define (car xs)
  (#car xs))

(define (cdr xs)
  (#cdr xs))

(define (number->char k)
  (#number->char k))

(define (char->number c)
  (#char->number c))

;; Print newline.
(define (newline)
  (display "\n"))

;; Invert boolean.
(define (not x)
  (if x #f #t))

;; Greater-than.
(define (> x y)
  (< y x))

;; Less-than-or-equal.
(define (<= x y)
  (not (< y x)))

;; Greater-than-or-equal.
(define (>= x y)
  (not (< x y)))

;; Empty list predicate.
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
