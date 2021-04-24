(let ((x 0)) (set! x 42) (display x))
(let ((count (let ((c 0)) (lambda () (set! c (+ 1 c)) c)))) (display (count)) (display (count)) (display (count)))
