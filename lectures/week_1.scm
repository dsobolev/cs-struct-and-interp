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

; #3 Takes a sentence as its argument and returns a sentence
; in which every instance of the words I or me is replaced by you, while every instance of
; you is replaced by me except at the beginning of the sentence, where it’s replaced by I
; Example: (switch ’(You told me that I should wake you up))  --->>> (i told you that you should wake me up)
(define (switch sentence)

	(define (switch_iter part)
		(if (eqv? '() (cdr part)) 
			;The end of a sentence. Last word
			(list (check_n_change (car part)))
			; Not the end. Do the change
			(cons 
				(check_n_change (car part)) 
				(switch_iter (cdr part)) )
		)
	)

	(define (check_n_change word)
		(cond 
			( (or (eqv? word 'i) (eqv? word 'me)) 'you )
			( (eqv? word 'you) 'me )
			( else word )
		)
	)

	(if (eqv? 'you (car sentence)) 
		(cons 'I (switch_iter (cdr sentence)) )
		(cons (car sentence) (switch_iter (cdr sentence)) )
	)
)

; #4 A predicate. 
; Takes a sentence of numbers.
; Returns a true value if the numbers are in ascending order, or a false value otherwise
(define (ordered? sentence)

	(define (check_asc val line)
		(if (eqv? '() line) 
			#t
			(if (> val (car line) )
				#f
				(check_asc (car line) (cdr line))
			)
		)
	)

	(if (or (eqv? '() sentence) (eqv? '() (cdr sentence)) )
		#f
		(check_asc (car sentence) (cdr sentence) )
	)
)