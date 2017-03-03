(ns sicp-clojure.core
  (:require [proto-repl.saved-values :as s]))


;;;;CODE FROM CHAPTER 1 OF STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;; Examples from the book are commented out with ; so that they
;;;  are easy to find and so that they will be omitted if you evaluate a
;;;  chunk of the file (programs with intervening examples) in Scheme.

;;; BEWARE: Although the whole file can be loaded into Scheme,
;;;  don't expect the programs to work if you do so.  For example,
;;;  the redefinition of + in exercise 1.9 wreaks havoc with the
;;;  last version of square defined here.


;;;SECTION 1.1.1

;; interpreter examples

; 486
;
; (+ 137 349)
; (- 1000 334)
; (* 5 99)
; (/ 10 5)
; (+ 2.7 10)
;
; (+ 21 35 12 7)
; (* 25 4 12)
;
; (+ (* 3 5) (- 10 6))
;
; (+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))
;
; (+ (* 3
;       (+ (* 2 4)
;          (+ 3 5)))
;    (+ (- 10 7)
;       6))


;;;SECTION 1.1.2

; (def size 2)
; size
; (* 5 size)
;
; (def pi 3.14159)
; (def radius 10)
; (* pi (* radius radius))
; (def circumference (* 2 pi radius))
; circumference


;;;SECTION 1.1.3

; (* (+ 2 (* 4 6))
;    (+ 3 5 7))

;;;SECTION 1.1.4

(defn square [x] (* x x))

; (square 21)

; (square (+ 2 5))
; (square (square 3))

(defn sum-of-squares [x y]
  (+ (square x) (square y)))

; (sum-of-squares 3 4)

(defn f [a]
  (sum-of-squares (+ a 1) (* a 2)))

(f 5)


;;;SECTION 1.1.5

; (f 5)
; (sum-of-squares (+ 5 1) (* 5 2))
; (+ (square 6) (square 10))
; (+ (* 6 6) (* 10 10))
; (+ 36 100)

; (f 5)
; (sum-of-squares (+ 5 1) (* 5 2))
; (+    (square (+ 5 1))      (square (* 5 2)))
; (+    (* (+ 5 1) (+ 5 1))   (* (* 5 2) (* 5 2)))
; (+         (* 6 6)             (* 10 10))
; (+           36                   100
;                     136)

;;;SECTION 1.1.6

(defn abs [x]
  (cond (> x 0) x
        (= x 0) 0
        (< x 0) (- x)))

(defn abs [x]
  (cond (< x 0) (- x)
        :else x))

(defn abs [x]
  (if (< x 0)
      (- x)
      x))

; (and (> x 5) (< x 10))

(defn >= [x y]
 (or (> x y) (= x y)))

(defn >= [x y]
  (not (< x y)))

;;EXERCISE 1.1
10

(+ 5 3 4)

(- 9 1)

(/ 6 2)

(+ (* 2 4) (- 4 6))

(def a 3)

(def b (+ a 1))

(+ a b (* a b))

(= a b)

(if (and (> b a) (< b (* a b)))
    b
    a)

(cond (= a 4) 6
      (= b 4) (+ 6 7 a)
      :else 25)

(+ 2 (if (> b a) b a))

(* (cond (> a b) a
         (< a b) b
         :else -1)
   (+ a 1))

;;Exc 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;; Ex 1.3

(defn smallest [a b]
  (if (< a b)
      a
      b))

(defn bob [a b c]
 (cond (< a (smallest b c)) (sum-of-squares b c)
       (< b (smallest a c)) (sum-of-squares a c)
       :else (sum-of-squares a b)))

;;EXERCISE 1.4
(defn a-plus-abs-b [a b]
  ((if (> b 0) + -) a b))

;; first evaluate (> b 0)
;; then evaluate if to give +/-
;; then apply this result


;;EXERCISE 1.5
(defn p []
  (p))

(defn test [x y]
  (if (= x 0)
      0
      y))

;; applicative order will give answer 0
;; evaluates cond, given true, then returns 0

;; normal order will attempt to replace y, which gives a recursive defintion
;; and kills the repl

; (test 0 (p))

;;;SECTION 1.1.7

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn average [x y]
  (/ (+ x y) 2))

(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn sqrt [x]
  (sqrt-iter 1.0 x))

; (sqrt 9)
; (sqrt (+ 100 37))
; (sqrt (+ (sqrt 2) (sqrt 3)))
; (square (sqrt 1000))

;;EXERCISE 1.6

(defn new-if [predicate then-clause else-clause]
  (cond predicate then-clause
        :else else-clause))

; (new-if (= 2 3) 0 5)
;
; (new-if (= 1 1) 0 5)

(defn sqrt-iter [guess x]
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

; What happens is a stack overflow
; The new-if tries to expand all clauses, where-as actual if only
; evaluates depending on the actual value of the statement

;; EXERCISE 1.7

; my good enough works on a percentage of the difference
; could be improved by usign a let in the iteration and passing through new guess

(defn good-enough? [guess x]
  (< (abs (- 1 (/ (square (improve guess x))
                  x)))
     0.0000001))

;;;SECTION 1.1.8

(defn square [x] (* x x))

(defn square [x]
  (Math/exp (double (Math/log x))))

(defn double [x] (+ x x))


;; As in 1.1.7
(defn sqrt [x]
  (sqrt-iter 1.0 x))

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn improve [guess x]
  (average guess (/ x guess)))


;; Block-structured
(defn sqrt [x]
  (defn good-enough? [guess x]
    (< (abs (- (square guess) x)) 0.001))
  (defn improve [guess x]
    (average guess (/ x guess)))
  (defn sqrt-iter [guess x]
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

;; Taking advantage of lexical scoping
(defn sqrt [x]
  (defn good-enough? [guess]
    (< (abs (- (square guess) x)) 0.001))
  (defn improve [guess]
    (average guess (/ x guess)))
  (defn sqrt-iter [guess]
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;;;SECTION 1.2.1

;; Recursive

(defn factorial [n]
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))


;; Iterative

(defn factorial [n]
  (fact-iter 1 1 n))

(defn fact-iter [product counter max-count]
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

;; Iterative, block-structured (from footnote)
(defn factorial [n]
  (defn iter [product counter]
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))


;;EXERCISE 1.9
(defn + [a b]
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(+ 3 4)
(inc (+ 2 4))
(inc (inc (+ 1 4)))
(inc (inc (inc (+ 0 4))))
(inc (inc (inc 4)))
(inc (inc 5))
(inc 6)
7

; This one is recursive

(defn + [a b]
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(+ 3 4)
(+ (dec 3) (inc 4))
(+ (dec 2) (inc 5))
(+ (dec 1) (inc 6))
7

; This one is iterative

;;EXERCISE 1.10
(defn A [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (A (- x 1)
                 (A x (- y 1)))))

(A 1 10)
(A 2 4)
(A 3 3)

(defn f [n]
  (A 0 n))

; f(n) = 2 * n

(defn g [n]
  (A 1 n))

; g(n) = 2 ** n
(g 2)
(A 1 2)
(A 0 (A 1 1))
(A 0 2)
(* 2 2)
4

(defn h [n]
  (A 2 n))

(h 2)
(A 2 2)
(A 1 (A 2 1))
(A 1 2)
(A 0 (A 1 1))
(A 0 2)
(* 2 2)
4

; f(n) = 2 ** n

(defn k [n]
  (* 5 n n))

; k(n) = 5 n**2

;;;SECTION 1.2.2

;; Recursive

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib (- n 1))
                 (fib (- n 2)))))

;; Iterative

(defn fib [n]
  (fib-iter 1 0 n))

(defn fib-iter [a b count]
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))


;; Counting change

(defn count-change [amount]
  (cc amount 5))

(defn cc [amount kinds-of-coins]
  (cond (= amount 0) 1
        (or (< amount 0) (= kinds-of-coins 0)) 0
        :else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins))))

(defn first-denomination [kinds-of-coins]
  (cond (= kinds-of-coins 1) 1
        (= kinds-of-coins 2) 5
        (= kinds-of-coins 3) 10
        (= kinds-of-coins 4) 25
        (= kinds-of-coins 5) 50))

(count-change 11)

;;EXERCISE 1.11

; recursive version
(defn f
  [n]
  (cond
    (< n 3) n
    :else (+ (f (- n 1))
             (* 2 (f (- n 2)))
             (* 3 (f (- n 3))))))
(f 1)
(f 4)
(f 5)
(f 6)

; iterative version

(defn g
  [n]
  (f-iter 1 2 3 n))

(defn f-iter
  [two-back one-back result count]
  (cond
    (< n 3) result
    :else (f-iter second third (+ first
                                  (* 2 second)
                                  (* 3 third))
                  (- n 1))))

(g 1)
(g 2)
(g 3)
(g 4)

; EXERCISE 1.2

(defn pascalvalue
  [row column]
  (s/save 1)
  (if (or (= 0 column) (= row column))
      1
      (+ (pascalvalue (- row 1) (- column 1))
         (pascalvalue (- row 1) column))))

(pascalvalue 0 0)
(pascalvalue 1 0)
(pascalvalue 1 1)
(pascalvalue 2 0)
(pascalvalue 2 1)
(pascalvalue 2 2)
(pascalvalue 3 0)
(pascalvalue 3 1)
(pascalvalue 3 2)
(pascalvalue 3 3)

;;;SECTION 1.2.3

;;Excercise 1.14

; steps order n
; space order n log n

;;EXERCISE 1.15
(defn cube [x]
  (* x x x))

(defn p [x]
  (s/save 2)
  (- (* 3 x) (* 4 (cube x))))

(defn sine [angle]
   (if (not (> (Math/abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

(sine 70.15)

; p is executed 5 times
; there is no change with angle being calculated, ie it is static

;;;SECTION 1.2.4

;; Linear recursion
(defn expt [b n]
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))


;; Linear iteration
(defn expt [b n]
  (expt-iter b n 1))

(defn expt-iter [b counter product]
  (if (= counter 0)
      product
      (expt-iter b
                (- counter 1)
                (* b product))))

;; Logarithmic iteration
(defn fast-expt [b n]
  (cond (= n 0) 1
        (even? n) (square (fast-expt b (/ n 2)))
        :else (* b (fast-expt b (- n 1)))))

(defn even? [n]
  (= (rem n 2) 0))


;;EXERCISE 1.17
(defn * [a b]
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

;;EXERCISE 1.19
(defn fib [n]
  (fib-iter 1 0 0 1 n))

(defn fib-iter [a b p q count]
  (cond (= count 0) b
        (even? count
         (fib-iter a
                   b
                   ??FILL-THIS-IN?? ; compute p'
                   ??FILL-THIS-IN?? ; compute q'
                   (/ count 2)))
        :else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1))))


;;;SECTION 1.2.5

(defn gcd [a b]
  (if (= b 0)
      a
      (gcd b (rem a b))))


;;;SECTION 1.2.6

;; prime?

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor n (+ test-divisor 1))))

(defn divides? [a b]
  (= (rem b a) 0))

(defn prime? [n]
  (= n (smallest-divisor n)))


;; fast-prime?

(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp)
        (rem (square (expmod base (/ exp 2) m))
             m)
        :else
         (rem (* base (expmod base (- exp 1) m))
              m)))

(defn fermat-test [n]
  (defn try-it [a]
    (= (expmod a n n) a))
  (try-it (+ 1 (rand-int (- n 1)))))

(defn fast-prime? [n times]
  (cond (= times 0) true
        (fermat-test n) (fast-prime? n (- times 1))
        :else false))


;;EXERCISE 1.22
(defn timed-prime-test [n]
  (newline)
  (println n)
  (start-prime-test n (System/currentTimeMillis)))

(defn start-prime-test [n start-time]
  (if (prime? n)
      (report-prime (- (System/currentTimeMillis) start-time))))

(defn report-prime [elapsed-time]
  (println " *** ")
  (println elapsed-time))

;;EXERCISE 1.25
(defn expmod [base exp m]
  (rem (fast-expt base exp) m))

;;EXERCISE 1.26
(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp)
        (rem (* (expmod base (/ exp 2) m)
                (expmod base (/ exp 2) m))
             m)
        :else
        (rem (* base (expmod base (- exp 1) m))
             m)))

;;;SECTION 1.3

(define (cube x) (* x x x))

;;;SECTION 1.3.1

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


;; Using sum

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

; (sum-cubes 1 10)


(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

; (sum-integers 1 10)


(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

; (* 8 (pi-sum 1 1000))


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

; (integral cube 0 1 0.01)

; (integral cube 0 1 0.001)


;;EXERCISE 1.32
; (accumulate combiner null-value term a next b)

;;;SECTION 1.3.2

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(define (plus4 x) (+ x 4))

(define plus4 (lambda (x) (+ x 4)))

; ((lambda (x y z) (+ x y (square z))) 1 2 3)


;; Using let

(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

; (+ (let ((x 3))
;      (+ x (* x 10)))
;    x)

; (let ((x 3)
;       (y (+ x 2)))
;   (* x y))

(define (f x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (square a))
     (* y b)
     (* a b)))


;;EXERCISE 1.34
(define (f g)
  (g 2))

; (f square)

; (f (lambda (z) (* z (+ z 1))))


;;;SECTION 1.3.3

;; Half-interval method

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))


; (half-interval-method sin 2.0 4.0)

; (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
;                       1.0
;                       2.0)


;; Fixed points

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


; (fixed-point cos 1.0)

; (fixed-point (lambda (y) (+ (sin y) (cos y)))
;              1.0)


(define (sqrt x)
  (fixed-point (lambda (y) (/ x y))
               1.0))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))


;;EXERCISE 1.37
; (cont-frac (lambda (i) 1.0)
;            (lambda (i) 1.0)
;            k)


;;;SECTION 1.3.4

(define (average-damp f)
  (lambda (x) (average x (f x))))

; ((average-damp square) 10)

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))


;; Newton's method

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)


(define (cube x) (* x x x))

; ((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))


(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))


;; Fixed point of transformed function

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))


;;EXERCISE 1.40
; (newtons-method (cubic a b c) 1)


;;EXERCISE 1.41
; (((double (double double)) inc) 5)


;;EXERCISE 1.42
; ((compose square inc) 6)


;;EXERCISE 1.43
; ((repeated square 2) 5)
