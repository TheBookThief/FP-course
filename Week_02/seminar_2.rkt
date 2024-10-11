#lang racket/base

(require racket/trace)

;Задача 1. Да се напише функция mygcd a b, която връща НОД(a, b).

(define (mygcd a b)
  (cond
        [(= a 0) b]
        [(= b 0) a]
        [(< a b) (mygcd a (remainder b a))]
        [(> a b) (mygcd b (remainder a b))]
        )
  )

(trace mygcd)

(mygcd 5 10)
(mygcd 5 3)
(mygcd 9 27)
(mygcd 11 121)
(mygcd 30 45)


;Задача 2. Да се напише функция mymaxdivisor x, която намира най-големия делител d на цялото число x > 1, за който d < x.

(define (mymaxdivisor x)
  (define (iter i)
    (cond
      [(= i x) 1]
      [(= 0 (remainder x i)) (/ x i)]
      [else (iter (+ 1 i))]
      )
    )
  (trace iter)
  (iter 2)
  )
(trace mymaxdivisor)
(mymaxdivisor 5)
(mymaxdivisor 100000)
(mymaxdivisor 10)
(mymaxdivisor 45)
        

;Задача 3. Да се дефинира функция sum-odds a b, която намира сумата на нечетните числа в затворения интервал [a, b].

(define (sum-odds a b)
  (define (iter i sum)
    (cond
      [(> i b) sum]
      [(= (remainder i 2) 1) (iter (+ i 1) (+ sum i))]
      [else (iter (+ i 1) sum)])
    )
  (iter a 0)
  )

(sum-odds 10 16)

;V2
(define (sum-odds-2 a b)
  (cond [(> a b) 0]
        [(= 1 (remainder a 2)) (+ a (sum-odds-2 (+ a 2) b))]
        [else (sum-odds-2 (+ a 1) b)])
)

(sum-odds-2 10 16)
             
;Задача 4. Да се дефинира предикат prime? n, който проверява дали естественото число n е просто.

(define (prime? n)
  (define (helper d)
    (cond [(= d n) #t]
          [(= 0 (remainder n d)) #f]
          [else (helper (+ d 1))])
    )
  (cond [(= n 1) #f]
        [(= n 2) #t]
        [else (helper 2)])
)


  
;Задача 5. Да се дефинира функция count-palindromes a b, която намира броя на палиндромите в интервала [a, b], където a и b са цели неотрицателни числа и a<b.

(define (count-palindromes a b)
  (define (reverse-num num rev)
    (if(< 0 num)
       (reverse-num (quotient num 10) (+ (* rev 10) (remainder num 10)))
       rev)
    )
  (define (iter i)
    (if (<= i b)
        (if (= i (reverse-num i 0))
            (+ 1 (iter (+ 1 i)))
            (iter (+ 1 i))
            )
        0
        )
   )
  (iter a)
  )
  
(count-palindromes 1 20)
    
    
  
;Задача 6. Да се дефинира функция count-divisors n, която чрез линейно итеративен процес намира броя на естествените делители на едно естествено число n.

(define (count-divisors n)
  (define (iter i cnt)
    (cond
      [(> i n) cnt]
      [(= 0 (remainder n i)) (iter (+ 1 i) (+ 1 cnt))]
      [else (iter (+ 1 i) cnt)])
    )
  (iter 1 0)
  )

(count-divisors 1)
(count-divisors 2)
(count-divisors 8)
(count-divisors 15)
(count-divisors 100)

