;daies_between_date.rkt
;计算两个日期之间的天数。

#lang racket

(require "leap_year.rkt")

(provide
 (contract-out
  [between-date-daies (-> year/c month/c day/c
                          year/c month/c day/c
                          integer?)]
  [month/c (-> any/c boolean?)]
  [day/c (-> any/c boolean?)]))

;以下定义合约检查函数--------------------------------
;检查月值：
(define (month/c month)
  (and (integer? month)
         (>= month 1)
         (<= month 12)))

;检查日值：
(define (day/c day)
  (and (integer? day)
         (>= day 1)
         (<= day 31)))

;以下定义计算函数------------------------------------
;定义大小月的月份：
;大月每月31天，小月每月30天，闰年二月29天，平常年二月28天。
(define big-month (vector 1 3 5 7 8 10 12)) ;31天
(define small-month (vector 4 6 9 11)) ;30天

;尾递归计算整年之间的天数：
;daies保存计算得到的天数值。
(define (between-years-daies year-begin year-end daies)
  (if (>= year-begin year-end)
      daies
      (between-years-daies
       (+ year-begin 1)
       year-end
       (+ daies
          (if (leap-year? year-begin)
              366 ;闰年
              365)))))

;计算指定月的整天数：
(define (month-daies year month)
  (cond
    [(vector-member month big-month) 31];为大月
    [(vector-member month small-month) 30];为小月
    [else ;为二月
     (if (leap-year? year)
         29 ;闰年
         28)]))

;计算不足月天数：
;flag标志为'front表示计算之前天数，'back表示计算之后天数。
(define (left-month-daies year month day tag)
  (let ([all-daies (month-daies year month)])
    (case tag
      ['front day]
      [else (- all-daies day)])))

;递归计算整月之间的天数：
;daies保存计算得到的天数值；
;flag标志为'begin表示不计算起始月天数，'end表示不计算结束月天数，'all表示均不计算，'none表示均计算。
(define (between-months-daies year month-begin month-end tag daies)
  (cond
    [(and (or (equal? tag 'begin )
              (equal? tag 'all))
          (= daies 0))
     (between-months-daies year
                           (+ month-begin 1)
                           month-end
                           (if (equal? tag 'begin)
                               'none
                               'end)
                           daies)]
    [(and (equal? tag 'end)
          (= month-begin month-end))
     daies]
    [else
     (if (> month-begin month-end)
      daies
      (between-months-daies year
                            (+ month-begin 1)
                            month-end
                            tag
                            (+ daies
                               (month-daies year month-begin))))]))

;计算指定年月日之当年余下的天数：
;tag标志为'front表示计算之前天数，'back表示计算之后天数。
(define (year-left-daies year month day tag)
  (case tag
    ['front
     (+ (left-month-daies year month day 'front)
        (between-months-daies year 1 month 'end 0))]
    [else
     (+ (left-month-daies year month day 'back)
        (between-months-daies year month 12 'begin 0))]))

;计算两个日期之间的间隔天数：
(define (between-date-daies year-begin month-begin day-begin
                            year-end month-end day-end)
  (cond
    [(> year-begin year-end) -1]
    [(= year-begin year-end)
     (cond
       [(> month-begin month-end) -1]
       [(= month-begin month-end)
        (cond
          [(> day-begin day-end) -1]
          [(= day-begin day-end) 1]
          [else (+ (- day-end day-begin) 1)])]
       [else
        (+ (left-month-daies year-begin month-begin day-begin 'back)
           (between-months-daies year-begin month-begin month-end 'all 0)
           (left-month-daies year-begin month-begin day-end 'front))])]
     [else
      (+ (year-left-daies year-begin month-begin day-begin 'back)
         (between-years-daies (+ year-begin 1) year-end 0)
         (year-left-daies year-end month-end day-end 'front))]))

;计算给定日期是星期几：
(define (week year month day)
  (let ([daies (between-date-daies 0 12 31 year month day)])
    (remainder daies 7)))