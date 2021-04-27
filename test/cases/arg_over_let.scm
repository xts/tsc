(define (id k)
  (let ((x 0))
    k))

; Verify that let bindings don't clobber arguments.
(display (id 42))
