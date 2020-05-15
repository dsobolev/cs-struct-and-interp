; Common sum procedure
(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a) 
		   (sum term (next a) next b))
	)
)

; Ex. 1.29 Simpson's Rule of numerical integration
; f - the actual function. cube us used for testing
; a, b - boundaries of an area
; n - number of iterations, should be even
(define (simpsons_integral f, a, b, n)
	(if (odd? n) (set! n (+ n 1)))

	(define (сoef i)
		(if (or (= 0 i) (= n i)) 
			1
			(if (even? i) 
				2
				4
			)
		)
	)

	(define term)
)

(define (cube x) (* x x x))