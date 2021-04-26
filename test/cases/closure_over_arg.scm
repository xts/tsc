(let ((f (lambda (x)
          (lambda ()
            x))))
  (display ((f 32))))
