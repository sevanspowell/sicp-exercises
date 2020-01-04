(define (square x) (* x x))
(square 21)
(square (+ 2 5))
(square (square 3))

(define (sum-of-squares x y)
  (+ (square x) (square y)))
(sum-of-squares 3 4)

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
(abs 0)

(define (abs-if x)
  (if (< x 0)
      (- x)
      x))
(abs-if 0)

;; Exc 1.1
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(= a a)
(if (and (> b a) (< b (* a b)))
    b
    a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

;; Exc 1.2
(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

;; Exc 1.3
(define (sum-square-two-largest x y z)
  (if (> x y)
      (if (> y z) (sum-of-squares x y) (sum-of-squares x z))
      (if (> x z) (sum-of-squares y x) (sum-of-squares y z))))
(sum-square-two-largest 1 2 3)
(sum-square-two-largest 1 3 2)
(sum-square-two-largest 3 1 2)
(sum-square-two-largest 3 2 1)
(sum-square-two-largest 2 1 3)
(sum-square-two-largest 2 3 1)

;; Exc 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(a-plus-abs-b 4 3)
(a-plus-abs-b 4 (- 3))

;; Exc 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

(test 0 (p))
;; When using applicative-order evaluation, Ben will observe an
;; infinite-loop. The '0' and 'p' expressions will be evaluated first.
;; When the 'p' expression is evaluated, the interpreter will enter an
;; infinite-loop, as p is recursively defined in terms of itself and
;; has no termination case.

;; When using normal-order evaluation, Ben will see that the
;; expression returns 0. First, test will be expanded and the
;; predicate of the 'if' special form will be evaluated first. Since
;; 'x' is equal to 0, the consequent 0 will be returned and the
;; alternative 'y' (which is 'p' in this case), will be un-evaluated,
;; avoiding the infinite loop.

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Exercise 1.6
;; An infinite loop will occur. Scheme is strict in the arguments of
;; function application (applicative-order evaluation). This means
;; that both the consequent and alternative will be evaluated in the
;; "new-if" function. This is a problem because "sqrt-iter"
;; is recursive . Even if the guess is "good enough" sqrt-iter will
;; continue to be called.

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (new-sqrt-iter (improve guess x) x)))

;; Exercise 1.8

;; (define (cube x) (* x x x))

;; (define (improve-cube guess x)
;;   (/ (+ (/ x (square guess)) (* 2 guess)) 3))

;; (define (good-enough-cube? guess x)
;;   (< (abs (- (cube guess) x)) 0.001))

;; (define (cubert-iter guess x)
;;   (if (good-enough-cube? guess x)
;;       guess
;;       (cubert-iter (improve-cube guess x) x)))

;; (define (cubert x)
;;   (cubert-iter 1.0 x))
      
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (fact n)
  (define (iter product counter max-count)
    (if (> counter max-count)
        product
        (iter (* counter product)
                   (+ counter 1)
                   max-count)))
  (iter 1 1 n))

;; Exercise 1.9
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

;; This process is recursive.

(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9

;; This process is iterative.

;; Exercise 1.10

(A 1 2)
(A 0 (A 1 1))
(* 2 2)

(A 1 n)
(A 0 (A 1 (- n 1)))
(* 2 (A 1 (- n 1)))
(* 2 (A 0 (A 1 (- n 1 1))))
(* 2 (* 2 (A 1 (- n 1 1))))
(* 2 (* 2 2))
(* 2 2 2)

(A 2 2)
(A 1 (A 2 1))
(A 1 2)
(A 0 (A 1 1))
(A 0 2)
(* 2 2)

(A 2 3)
(A 1 (A 2 2))
(A 1 (A 1 (A 2 1)))
(A 1 (A 1 2))
(A 1 (A 0 (A 1 1)))
(A 1 (A 0 2))
(A 1 (* 2 2))
(A 0 (A 1 (- (* 2 2) 1)))
(A 0 (A 0 (A 1 2)))
(A 0 (A 0 (A 0 (A 1 1))))
(A 0 (A 0 (A 0 2)))
(* 2 (* 2 (* 2 2)))
(* 2 2 2 2)

;; (f n) computes 2*n
;; (g n) computes 2^n
;; (h n) computes 2^2...(n-1 times)
;; (k n) computes 5n^2
