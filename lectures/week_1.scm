; #2 Takes sequence of numbers and returns its squares
(define (squares l)
	(build_sequence l '())
)

(define (sq x) (* x x) )

(define (square_first l) (sq (car l) ) )

(define (append_squared res l) 
	(append res (list (square_first l) ) ) 
)

(define (build_sequence inp res)
	(set! res (append_squared res inp) ) 
	(if (eqv? '() (cdr inp) )
		res 
		(build_sequence (cdr inp) res)
	)
)