#lang racket/base

;Задача 1. Да се напише функция mymin, която приема два аргумента и връща по-малкия от тях.
(define (mymin x y)
(if (< x y)
x
y))

(mymin 5 4) ;4
(mymin 1 8) ;1

;Задача 2. Да се дефинира функцията inside? x a b, която проверява дали числото x се намира в затворения интервал [a, b].

(define (inside? x a b)
(if(and (>= x a) (<= x b))
#t
#f
))

;V2
;(define (inside? x a b)
;  (and (>= x a) (<= x b)))

(inside? 2 1 5) ;1
(inside? 2 5 7) ;0

;Задача 3. Да се напише функция myfunc, която пресмята средно аритметично на квадратите на 2 числа.

(define (average a b) (/ (+ a b) 2))
(define (square a) (* a a))

(define (avg_square a b) (average (square a) (square b)))

;V2
(define (myfunc a b)
  (define (sqr a)
    (* a a))
  (define (average a b)
    (/ (+ a b) 2))
   (average sqr(a) sqr(b)))

(avg_square 1 3) ;5
(avg_square 5 6) ;61/2


;Задача 4. Да се напише myfib, която получава един аргумент n и връща n-тото число на Фибоначи. 
;Да се напише и итеративно решение.

;(Заб.: редицата е 1, 1, 2, 3, 5, ... и е индексирана от 0.)

(define (fib_calc cur prev idx)
(if (<= idx 0)
cur
(fib_calc (+ cur prev) cur (- idx 1))
)
)

;V2
(define (fact-iter n)
  (define (iter res i)
    (if (> i n)
        res
        (iter (* res i) (+ i 1))
        )
    )
  (iter 1 1)
 )
(fact-iter 5)

(define (fib n) (fib_calc 1 1 (- n 1)))

;V2
(define (fib-iter n)
  (define (iter prev cur i)
    (if (>= i n)
        cur
        (iter cur (+ cur prev) (+ i 1))
        )
    )
  (iter 1 1 1))

(fib-iter 0)
(fib-iter 1)
(fib-iter 2)
(fib-iter 5)

(fib 0) ;1
(fib 1) ;1 
(fib 2) ;2
(fib 3) ;3
(fib 18) ;3
(fib 39) ;102334155

