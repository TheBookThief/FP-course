#lang racket

(require math/number-theory)


; Задача 1. Да се дефинира функция (sum-prime-divisors n),
; която намира сумата на всички прости делители на едно число n.

; V1
(define (sum-prime-divisors n)
  (define (helper d)
    (cond [(> d n) 0]
          [(and (prime? d) (= (remainder n d) 0)) (+ d (helper (+ d 1)))]
          [else (helper (+ d 1))]
          )
    )
  (helper 1)
  )

; V2 linearly iterative

(define (sum-prime-divisors-2 n)
  (define (helper d sum)
    (cond [(> d n) sum]
          [(and (prime? d) (= (remainder n d) 0)) (helper (+ d 1) (+ sum d))]
          [else (helper (+ d 1) sum)]
          )
    )
  (helper 1 0)
  )

(sum-prime-divisors 10)
(sum-prime-divisors-2 10)


; V3 with lists
(inclusive-range 1 10)
(define (sum-prime-divisors-map n)
  (foldl + 0 (filter (lambda (d) (and (= 0 (remainder n d))
                                 (prime? d)))
                     (inclusive-range 1 n)))
  )

(sum-prime-divisors-map 10)
; Задача 2. Да се дефинира функция (pow x n),
; която генерира линейно рекурсивен процес и намира x на степен n,
; където x е реално, а n - естествено число.

(define (pow-r x n)
  (if (= n 0)
      1
      (* x (pow-r x (- n 1)))
      )
  )
(pow-r 2 4)

(define (pow x n)
  (define (helper it res)
    (cond [(> it n) res]
          [else (helper (+ it 1) (* res x))]
          )
    )
  (helper 1 1)
  )
(pow 2 4)

; Задача 3. Да се дефинира функция (count-оccurences d n),
; намираща броя на срещанията на дадена цифра d в записа на число n.

; V1
(define (count-occurances-2 d n)
  (cond [(< n 10) (if (= n d) 1 0)]
        [(= d (remainder n 10)) (+ 1 (count-occurances-2 d (quotient n 10)))]
        [else (count-occurances-2 d (quotient n 10))])
  )
(count-occurances-2 2 12222344)

; V2
(define (number->list n)
  (define (helper n)
    (if (< n 10)
        (list n)
        (cons (remainder n 10)
               (helper (quotient n 10)))))
  (reverse (helper n)))

(number->list 12234)

(define (count-occurances d n)
  (length (filter (lambda (k) (= k d))(number->list n))))

(count-occurances 2 12222344)

;number->list

;integers are counted on char value indexing
(map char->integer (string->list (number->string 12234)))

;convert to the real int values
(define (number->list-2 n)
(map (lambda (c) (- (char->integer c) (char->integer #\0)))
     (string->list (number->string n))))

(number->list-2 12234)

; Задача 4. Да се дефинира предикат (ascending? n), който връща истина,
; ако цифрите на дадено естествено число n са в нарастващ ред
; от първата към последната.

; V1

(define (ascending? n)
  (or (< n 10)
      (and (< (remainder (quotient n 10) 10)
              (remainder n 10))
           (ascending? (quotient n 10)))))

(ascending? 12345)
(ascending? 12342)

; V2

(define (ascending-2? n)
  (define lst (number->list n))
  (andmap < (drop-right lst 1) (rest lst)))

(ascending-2? 12345)
(ascending-2? 12342)

; Задача 5. Да се дефинира функцията (perfect-number? n),
; която проверява дали числото n e съвършено, т.е.
; дали е равно на сбора на делителите си.

(define (perfect-number? n)
  (= n (foldl + 0 (filter (lambda (d) (= (remainder n d) 0)) (range 1 n)))))

(define (perfect-number-2? n)
  (= n (apply + (filter (lambda (d) (= (remainder n d) 0)) (range 1 n)))))

(perfect-number? 6)
(perfect-number? 7)
(perfect-number? 8)

; Задача 6. По зададени x и n, да се дефинира функция
; (calc-sum x n), която пресмята сумата: 1 + x + x^2 + x^3 + ... + x^n.
; Използвайте не повече от n на брой умножения.

(define (calc-sum x n)
  (define (calc-sum-helper x n cur)
    (if (= n 0)
        cur
        (calc-sum-helper x (- n 1) (+ 1 (* x cur)))))
  (calc-sum-helper x n 0)
  )

(calc-sum 1 5)
(calc-sum 2 6)














