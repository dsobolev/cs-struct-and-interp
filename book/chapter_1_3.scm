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
; Calc п/4. As I understand, the idea is: п/4 = 2/3 * 4/3 * 4/5 * 6/5 ...
; kinda a/b * (a + 2)/b * (a + 2)/(b + 2) and so on.
; In the book it is rewriten as (2*4*4*6...)/(3*3*5*5..)
; I suppose the denominator to be (1*3*3*5...)
(define (one_fourth_of_pi highest)

    (define (term x) (* x (+ x 2)) )
    (define (next x) (+ x 2) )

    (if (even? highest)
        (/ (product term 2 next highest)
           (* (product term 1 next (- highest 1)) (+ highest 1))
        )
        (/ (* (product term 2 next highest) (+ highest 1) )
           (product term 1 next highest)
        )
    )
)

(define (approx_pi highest)
    (* 4 (one_fourth_of_pi highest) )
)

;Ex 1.32a 'sum' and 'product' are the cases of 'accumulate'
(define (accumulate combiner null_value term a next b)
    (if (> a b)
        null_value
        (combiner (term a)
                  (accumulate 
                      combiner 
                      null_value 
                      term 
                      (next a) 
                      next 
                      b)
        )
    )
)

(define (sum_ac term a next b)
    (accumulate + 0 term a next b)
)

(define (product_ac term a next b)
    (accumulate * 1 term a next b)
)

(define (factorial n)
    (define (next x) (+ 1 x))
    (define (term x) x)

    (product_ac term 1 next n)
)

;Ex 1.33 Accumulate only those values which satisfy some 'filter' condition
(define (filtered_accumulate filter combiner null_value term a next b)
    (if (> a b)
        null_value
        (if (filter a)
            (combiner (term a)
                      (filtered_accumulate filter combiner null_value term (next a) next b))
            (combiner null_value
                      (filtered_accumulate filter combiner null_value term (next a) next b))
        )
    )
)

;# Ex 1.33a Sum of squares of prime numbers in [a, b] interval
(define (prime? x)

    (define (divided? a b) (= 0 (remainder a b)) )

    (define (prime_iter arg)
        (if (= arg 1)
            #f
            (if (> arg (floor (/ x 2)) )
                #t
                (if (divided? x arg)
                    #f
                    (prime_iter (+ arg 1)))))
    )

    (prime_iter 2)
)

(define (sum_of_squares_prime a b)
    (define (square x) (* x x))
    (define (next x) (+ x 1))

    (filtered_accumulate prime? + 0 square a next b )
)

;Ex 1.33b Product of positive integers (x < N) that are relatively prime to N
(define (primes_list n)

    (define (check_prime x)
        (if (> x n)
            '()
            (if (prime? x)
                (cons x (check_prime (+ x 1)))
                (check_prime (+ x 1))
            )
        )
    )

    (cons 2 (check_prime 3))
)

(define (prime_divisors x)

    (define (divisor? div)
        (= 0 (remainder x div)))

    (define (check_divisors lst)
        (if (eqv? '() lst)
            '()
            (if (divisor? (car lst))
                (cons (car lst) (check_divisors (cdr lst)))
                (check_divisors (cdr lst)))
        )
    )

    (check_divisors ( primes_list (floor (/ x 2)) ) )
)
; 1) N is not divided to X
; 2) N and X don't have common divisors