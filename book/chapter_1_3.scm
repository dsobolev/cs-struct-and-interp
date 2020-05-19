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
(define (simpsons_integral f a b n)
    (if (odd? n) (set! n (+ n 1)))

    (define h (/ (- b a) n))

    (define (coef i)
        (if (or (= 0 i) (= n i)) 
            1
            (if (even? i) 
                2
                4
            )
        )
    )

    (define (arg_val i) (+ a (* i h)))

    (define (sum_set i)
        (if (> i n)
            0
            (+ (* (coef i) 
                  (f (arg_val i)) ) 
               (sum_set (+ i 1))
            )
        )
    )

    (* (/ h 3)
       (sum_set 0))
)

(define (cube x) (* x x x))

; To check simpsons_integral
(simpsons_integral cube 0 1 100)
(simpsons_integral cube 0 1 1000)

;Ex 1.31a 
; Common product procedure
(define (product term a next b)
    (if (> a b) 
        1
        (* (term a)
           (product term (next a) next b))
    )
)