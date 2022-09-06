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
; Time taken: about 40 mins
; [contract] Vehicle 
; [purpose] To define the type Vehicle, which has three variants, Bicycle, Car, Airplane.
; [tests] (test (Bicycle? (define myBike (3)))
;           

(define-type Vehicle
	[Bicycle 		(wheels number?)]
	[Car 	(wheels number?)
				(windows number?)]
	[Airplane 	(wheels number?)
				(windows number?)
                                (engines number?)])
(define myBike (Bicycle 3))

(test (Bicycle? myBike) true)
      
; Problem 6-b
; Solved by myself: Y
; Time taken: about 20 mins
; [contract] vehicle-tax : Vehicle numver number number -> number
; [purpose] To calculate tax for a vehicle differs on the number of wheels, windows and engines.
; [tests] 
;          

(define (vehicle-tax v tax_per_wheel tax_per_window tax_per_engine)
	(type-case Vehicle v
		[Bicycle (wh)	(* wh tax_per_wheel)]
		[Car (wh wi)	(+(* wh tax_per_wheel) (* wi tax_per_window))]
		[Airplane (wh wi en) (+(* wh tax_per_wheel) (* wi tax_per_window) (* wi tax_per_engine))]
          ))

; Problem 6-c
; Solved by myself: Y
; Time taken: about 20 mins
; [contract] is-vehicle-safe : Vehicle -> string
; [purpose] To calculate tax for a vehicle differs on the number of wheels, windows and engines.
; [tests] 
;          

(define (is-vehicle-safe v )
	(type-case Vehicle v
		[Bicycle (wh)
                         (cond
                           [(< wh 4) "safe"]
                           [else "unsafe"])]
          
		[Car (wh wi)
                     (cond
                       [(and((< 3 wh) (< 2 wi))) "safe"]
                       [else "unsafe"])]
          
		[Airplane (wh wi en)
                          (cond
                            [(and (< 2 wh) (< 10 wi) (< 1 en) ) "safe"]
                            [else "unsafe"])]
          ))