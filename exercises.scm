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

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(a-plus-abs-b 4 3)
(a-plus-abs-b 4 (- 3))

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
