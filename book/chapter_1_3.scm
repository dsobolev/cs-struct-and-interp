; Common sum procedure
(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a) 
		   (sum term (next a) next b))
	)
)

; Ex. 1.29 Simpson's Rule of numerical integration
(define (cube x) (* x x x))