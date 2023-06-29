#lang sicp
;; Exercise 1.7
(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y)
     2))

(define (improve guess x)
  (average guess (/ x guess)))

;iterates until guess and next guess are equal, automatically produces answer to limit of system precision 
(define (good-enough? guess x)
   (= guess (improve guess x)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; --------------------------------
;; Exercise 1.8
(define (cbrt-iter guess x)
  (if (cbrt-good-enough? guess x)
      guess
      (cbrt-iter (newtons-method-improve guess x) x)))

(define (newtons-method-improve guess x)
  (/ (+ (/ x
           (square guess))
        (* 2 guess))
     3))

(define (cbrt-good-enough? guess x)
  (= (newtons-method-improve guess x)
     guess)) 

;change initial guess to 1.1 to prevent an anomolous result for cube root of -2 
(define (cbrt x)
  (cbrt-iter 1.1 x))

