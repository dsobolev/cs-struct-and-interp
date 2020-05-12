; #2 Takes sequence of numbers and returns its squares
; It might be var from ideal since I don't know yet how to 
; use variables right and how to accumulate values properly
(define (squares input_list)

	(define (build_sequence inp res)

		(set! res (append_squared res inp) ) 

		(if (eqv? '() (cdr inp) )
			res 
			(build_sequence (cdr inp) res)
		)
	)

	(define (append_squared res lst) 
		(append res (list (square_first lst) ) ) 
	)

	(define (square_first lst) (sq (car lst)))

	(define (sq x) (* x x))

	(build_sequence input_list '())
)
