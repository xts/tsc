(let ((f (lambda () (let ((x 42)) (lambda () x))))) (display ((f))))
