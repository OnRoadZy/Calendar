;julian-date.rkt
;根据格里历计算儒略历。

#lang racket

(require "leap_year.rkt"
         "daies_between_date.rkt")

(provide
 (contract-out
  [julian-date (-> year/c month/c day/c positive? positive? positive? real?)]
  [gregorian-date? (-> year/c month/c day/c boolean?)]))

;计算儒略历参数：
;a：
(define (a month)
  (/ (- 14 month) 12))
;y：
(define (y year month)
  (+ year 4800 (- (a month))))
;m：
(define (m month)
  (+ month (* 12 (a month)) (- 3)))

;计算格里历（正午12:00）对应的儒略历：
(define (julian-date year month day hour minute second)
  (let* ([jdn (+ day
                 (/ (+ (* 153 (m month)) 2) 5)
                 (* 365 (y year month))
                 (/ (y year month) 4))]
         [jdn1 (if (gregorian-date? year month day)
                   (+ jdn
                      (- (/ (y year month) 100))
                      (+ (/ (y year month) 400))
                      (- 32045.5))
                   (- jdn 32083.5))])
    (+ jdn1
       (/ hour 24.0)
       (/ minute 1440.0)
       (/ second 86400.0))))

;判断日期是适用于格里历：
(define (gregorian-date? year month day)
  (cond
    [(> year 1582) #t]
    [(< year 1582) #f]
    [else
     (cond
       [(> month 10) #t]
       [(< month 10) #f]
       [else
        (cond
          [(>= day 15) #t]
          [else #f])])]))
       