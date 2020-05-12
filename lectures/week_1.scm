; #2 Takes sequence of numbers and returns its squares
(define (squares l)
	()
)

(define (sq x) (* x x) )

(define (square_first l) (sq (car l) ) )

(define (append_squared res l) 
	(append res (list (square_first l) ) ) 
)

(define (build-seq inp res)
	(set! res (append_squared res inp) ) 
	(if (eqv? '() (cdr inp) )
		res 
		(build-seq (cdr inp res) )
	)
)