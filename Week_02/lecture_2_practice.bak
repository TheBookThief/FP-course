#lang racket/base
(define (inc x)
  (+ x 1))

(define (sqr a)
  (* a a))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))
        )
    )
  (iter a 0)
  )

(sum-iter sqr 1 inc 5)
(sum-iter (lambda (x) (* x x)) 1 (lambda (x) (+ x 1)) 5)

(define cube (lambda (x) (* x x x)))

(cube 5)

(define (boundry priority)
  (if (> priority 5)
      (lambda (a b) (+ a b))
      (lambda (a b) (* a b))
      )
  )
(boundry 10)
((boundry 10) 1 2)
((boundry 1) 1 2)

(define (f x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (* a a) (* y b) (* a b)))
  )

(f 1 2)

(define x1 2)
(let ([x1 3] [y1 (+ x1 2)]) (* x1 y1))

(let* ([x 3] [y (+ x 2)]) (* x y))

(letrec ([fact (lambda (n)
                 (if(= n 1) 1 (* n (fact (- n 1)))))]) (fact 7))

(define (fact n)
  (letrec ([fact-iter (lambda (arg res)
                       (if (= arg 1)
                           res
                           (fact-iter (- arg 1)
                                      (* res arg))))])
    (fact-iter n 1)))

(fact 7)

(define (even-odd? n)
  (letrec ([odd? (lambda (x)
                   (and (not (= x 0)) (even? (- x 1))))]
           [even? (lambda (x)
                    (or (= x 0) (odd? (- x 1))))]
           )
    (odd? n))
  )
(even-odd? 10)
(even-odd? 11)

