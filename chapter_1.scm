; Ex. 1.3
; Takes 3 args
; Returns the sum of the squares of the two larger numbers
(define (maxsum a1 a2 a3)
	(define (square x)
		(* x x)
	)
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

; Ex. 1.7
; Square root procedure by Newton's Method.
(define (sqrt-custom x tolerance)
	
	; Sqrt iterative with a value tolerance set
	(define (sqrt-iter guess)
		(if (good-enough? guess (improve guess))
			guess
			(sqrt-iter (improve guess))
		)
	)

	; Compares values diff with tolerance previously set
	(define (good-enough? a b)
		(< (abs (- a b)) tolerance) )

	; Improves value of 'guess' based on main arg value
	(define (improve guess)
		(average guess (/ x guess) ))

	; Average of two args
	(define (average a b)
		(/ (+ a b) 2) ) 

	(sqrt-iter 1.0)
)

; Ex. 1.8
; Cube root by Newton's method. 
; Approximation formula (x - arg, guess - approximation value): ( x/(guess*guess) + 2*guess ) / 3
(define (improve x guess) 
	(/ (+ (/ x (* guess guess) ) (* 2 guess) ) 3 ))