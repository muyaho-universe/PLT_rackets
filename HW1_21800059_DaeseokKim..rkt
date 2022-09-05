#lang plai

; Problem 1
; Solved by myself: Y
; Time taken: about 10 mins
; [contract] dollar->won number -> number
; [purpose] To convert US dollar to Korea won
; [tests] (test (dollar->won 1) 1342)
;           (test (dollar->won 2) 2684)
(define (dollar->won dollar)
  (* dollar 1342))
(test (dollar->won 1) 1342)
(test (dollar->won 2) 2684)

; Problem 2
; Solved by myself: Y
; Time taken: about 10 mins
; [contract] digit_sum: number number number -> number
; [purpose] To sum three integer numbers
; [tests] (test (digit_sum 1 2 3) 6)
;           (test (digit_sum 10 20 30) 266084)
(define (digit_sum num1 num2 num3)
  (+ (+ num1 num2) num3))
(test (digit_sum 1 2 3) 6)
(test (digit_sum 10 20 30) 60)

; Problem 3
; Solved by myself: Y
; Time taken: about 20 mins
; [contract] volume-sphere : number -> number
; [purpose] To produce the volume of the sphere by given radius
; [tests] (test (volume-sphere 3) 113.09733552923255)
;           (test (volume-sphere 10) 4188.790204786391)
(define (volume-sphere radius)
  (/(* (expt radius 3) 4 pi) 3))

(test (volume-sphere 3) 113.09733552923255)
(test (volume-sphere 10) 4188.790204786391)

; Problem 4
; Solved by myself: Y
; Time taken: about 10 mins
; [contract] is-even? : number -> boolean
; [purpose] To determine whether n is even number
; [tests] (test (is-even? 3) false)
;           (test (is-even? 10) true)

(define (is-even? n)
  (=(modulo n 2)0))

(test (is-even? 3) false)
(test (is-even? 10) true)

; Problem 5
; Solved by myself: Y
; Time taken: about 10 mins
; [contract] combination : number -> number
; [purpose] To return the number of combinations that can be there
; [tests] (test (combination 3 2) 3)
;           (test (combination 10 4) 210)

(define (factorial n)
     (cond
       [( = n 0 )1]
       [else (* n (factorial (- n 1 )))]
        ))

(define (combination n k)
  (/(factorial n) (*(factorial(- n k))(factorial k))))

(test (combination 10 4) 210)
(test (combination 3 2) 3)

; Problem 6-a
; Solved by myself: Y
; Time taken: about 10 mins
; [contract] Vehicle : number -> number
; [purpose] To define the type Vehicle, which has three variants, Bicycle, Car, Airplane.
; [tests] (test (combination 3 2) 3)
;           (test (combination 10 4) 210)

(define-type GUI
	[label 		(text string?)]
	[button 	(text string?)
				(enabled? boolean?)]
	[choice 	(items (listof string?))
				(selected integer?)])
