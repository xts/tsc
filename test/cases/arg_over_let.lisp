; Verify that let bindings don't clobber arguments.
(define id (k)
  (let ((x 0))
    k))

(display (id 42))
