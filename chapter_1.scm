# Ex. 1.3
# Takes 3 args
# Returns the sum of the squares of the two larger numbers
(define (maxsum a1 a2 a3)
	(if (> a1 a2) 
		(if (> a2 a3) 
			(+ (square a1) (square a2) )
			(+ (square a1) (square a3) ) 
		)
		(if (> a1 a3)
			(+ (square a1) (square a2) )
			(+ (square a3) (square a2) )
		)
	)
)

(define (square x)
	(* x x))