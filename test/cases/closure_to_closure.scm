; Ensure that we can pass a value from one closure to another. In this case
; the prelude newline has been captured by the outer lambda and needs to be
; passed to the inner lambda.
(define (magic)
  91)

(define (outer)
  (lambda ()
    (display (magic))))

((outer))
