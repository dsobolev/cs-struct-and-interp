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
	(
		; Sqrt iterative with a value tolerance set
		(define (sqrt-iter x guess tolerance)
			(if (good-enough? guess (improve guess x) tolerance)
				guess
				(sqrt-iter x (improve guess x) tolerance)
			)
		)

		; Compares values diff with proper delta
		(define (good-enough? a b delta)
			(< (abs (- a b)) delta) )

		; Improves value of 'guess' based on main arg value
		(define (improve guess x)
			(average guess (/ x guess) ))

		; Average of two args
		(define (average a b)
			(/ (+ a b) 2) ) 

		(sqrt-iter x 1.0 tolerance)
	)
)
