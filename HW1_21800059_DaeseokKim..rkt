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
; [tests] (test (Bicycle? myBike) true)
;           (test (Bicycle? myCar) false)
;           (test (Airplane? badAirplane) true)

(define-type Vehicle
	[Bicycle 		(wheels number?)]
	[Car 	(wheels number?)
				(windows number?)]
	[Airplane 	(wheels number?)
				(windows number?)
                                (engines number?)])
(define myBike (Bicycle 3))
(define myCar (Car 4 4))
(define badCar (Car 1 4))
(define myAirplane (Airplane 4 100 4))
(define badAirplane (Airplane 1 0 4))

(test (Bicycle? myBike) true)
(test (Bicycle? myCar) false)
(test (Airplane? badAirplane) true)
      
; Problem 6-b
; Solved by myself: Y
; Time taken: about 20 mins
; [contract] vehicle-tax : Vehicle numver number number -> number
; [purpose] To calculate tax for a vehicle differs on the number of wheels, windows and engines.
; [tests] (test (vehicle-tax myBike 10 20 30) 30)
;           (test (vehicle-tax myCar 100 150 200) 1000)
;           (test (vehicle-tax badAirplane 1500 3000 5000) 21500)

(define (vehicle-tax v tax_per_wheel tax_per_window tax_per_engine)
	(type-case Vehicle v
		[Bicycle (wh)	(* wh tax_per_wheel)]
		[Car (wh wi)	(+(* wh tax_per_wheel) (* wi tax_per_window))]
		[Airplane (wh wi en) (+(* wh tax_per_wheel) (* wi tax_per_window) (* en tax_per_engine))]
          ))

(test (vehicle-tax myBike 10 20 30) 30)
(test (vehicle-tax myCar 100 150 200) 1000)
(test (vehicle-tax badAirplane 1500 3000 5000) 21500)

; Problem 6-c
; Solved by myself: Y
; Time taken: about 20 mins
; [contract] is-vehicle-safe : Vehicle -> string
; [purpose] To make sure that all vehicles meet the safety standards.
; [tests] (test (is-vehicle-safe myBike) "safe")
;           (test (is-vehicle-safe myCar) "safe")
;           (test (is-vehicle-safe badCar) "unsafe")
;           (test (is-vehicle-safe myAirplane) "safe")
;           (test (is-vehicle-safe badAirplane) "unsafe")   

(define (is-vehicle-safe v )
	(type-case Vehicle v
		[Bicycle (wh)
                         (cond
                           [(< wh 4) "safe"]
                           [else "unsafe"])]
          
		[Car (wh wi)
                     (cond
                       [(and(< 3 wh) (< 2 wi)) "safe"]
                       [else "unsafe"])]
          
		[Airplane (wh wi en)
                          (cond
                            [(and (< 2 wh) (< 10 wi) (< 1 en) ) "safe"]
                            [else "unsafe"])]
          ))

(test (is-vehicle-safe myBike) "safe")
(test (is-vehicle-safe myCar) "safe")
(test (is-vehicle-safe badCar) "unsafe")
(test (is-vehicle-safe myAirplane) "safe")
(test (is-vehicle-safe badAirplane) "unsafe")


; Problem 7
; Solved by myself: Y
; Time taken: about 60 mins
; [contract] update-name : symbol symbol list of symbols -> list of symbols
; [purpose] To replace all occurrences of old by new
; [tests] (test (update-name 'cherry 'claire (cons 'jc (cons 'cherry (cons 'kate empty))))
;      '(jc claire kate))
;(test (update-name 'muyaho 'ball (list 'fifa 'soccer 'muyaho 'goal))
;     '(fifa soccer ball goal))
;(test (update-name 'abc 'zzz (list 'abc 'def 'ghi 'jkl))
;     '(zzz def ghi jkl))

;(test (update-name 'jkl 'zzz (list 'abc 'def 'ghi 'jkl))
;     '(abc def ghi zzz))

(define (update-name old new list_of_symbols)
  (append(foldl cons '() (rest (member old (foldl cons '() list_of_symbols))))
  (cons new (rest (member old list_of_symbols))))
 )

(test (update-name 'cherry 'claire (cons 'jc (cons 'cherry (cons 'kate empty))))
     '(jc claire kate))

(test (update-name 'muyaho 'ball (list 'fifa 'soccer 'muyaho 'goal))
     '(fifa soccer ball goal))

(test (update-name 'abc 'zzz (list 'abc 'def 'ghi 'jkl))
     '(zzz def ghi jkl))

(test (update-name 'jkl 'zzz (list 'abc 'def 'ghi 'jkl))
     '(abc def ghi zzz))

; Problem 8
; Solved by myself: Y
; Time taken: about 80 mins
; [contract] binary-search :a list of numbers number ->  list of numbers
; [purpose] To produces a list of numbers which is binary-search traversal history.
; [tests] (test (binary-search '(1 2 3) 3) '(2 3)) 
;         (test (binary-search '(1 2 3 4 5 6 7 8) 3) '(4 2 3))
;         (test(binary-search '(1 2 3 4 5 6 7 8 9 10) 9) '(5 8 9))
;         (test (binary-search '(1 2 3 4 5 6 7) 6) '(4 6))
;         (test (binary-search '(1 2 3 4 5 6 7 8 9) 4) '(5 2 3 4)) 
;         (test (binary-search '(1 2 3 4 5 6 7 8 9) 3) '(5 2 3))


(define (half-of-list lst)
  (cond
    [(= (modulo (length lst) 2) 1) (list-ref lst (exact-floor (/ (length lst) 2)))]
    [else (list-ref lst (- (/ (length lst) 2) 1))]
  )
 )



(define (binary-search-with-list lst target memory-lst)
  (cond
    [(= (half-of-list lst) target)
     (append memory-lst (cons (half-of-list lst) empty))
     ]
    [else (cond
            [(< (half-of-list lst) target) (binary-search-with-list (drop lst (exact-floor (/ (length lst) 2))) target (append memory-lst (cons (half-of-list lst) empty)))] ;to right
            [else  (binary-search-with-list (take lst (exact-floor (/ (length lst) 2))) target (append memory-lst (cons (half-of-list lst) empty)))] ; to left
            )]
    )
  )

(define (binary-search lst target)
  (cond
    [(= (half-of-list lst) target)
     (list (half-of-list lst))
     ]
    [else (cond
            [(< (half-of-list lst) target) (binary-search-with-list (cond
                                                                      [(= (modulo (length lst) 2) 1) (drop lst (+ (exact-floor (/ (length lst) 2))1))]
                                                                      [else (drop lst  (/ (length lst) 2) )]
                                                                     
                                                                      ) target (cons (half-of-list lst) empty))] ;to right
            [else  (binary-search-with-list (cond
                                                                      [(= (modulo (length lst) 2) 1) (take lst  (exact-floor (/ (length lst) 2)))]
                                                                      [else (take lst  (/ (length lst) 2) )]
                                                                     
                                                                      ) target (cons (half-of-list lst) empty))] ; to left
            )]
    )
  )

(test (binary-search '(1 2 3) 3) '(2 3)) 
(test (binary-search '(1 2 3 4 5 6 7 8) 3) '(4 2 3))
(test(binary-search '(1 2 3 4 5 6 7 8 9 10) 9) '(5 8 9))
(test (binary-search '(1 2 3 4 5 6 7) 6) '(4 6))
(test (binary-search '(1 2 3 4 5 6 7 8 9) 4) '(5 2 3 4)) 
(test (binary-search '(1 2 3 4 5 6 7 8 9) 3) '(5 2 3))
