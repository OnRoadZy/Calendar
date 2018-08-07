;zeller_week.rkt
;根据蔡勒公式计算星期几：
;蔡勒公式：w=(y+y/4+c/4-2c+13*(m+1)/5+d-1)%7 [1582.10.15之后]
;        w=(5-c+y+(y/4)+(13*(m+1)/5)+d-1)%7 [1582.10.14之前]
;其中：c为世纪数减1的值（如21世纪，则c=20）；
;     m为月数，3<=m<=14（1月2月看做上一年的13月14月）；
;     y为年份，取公元纪年的后两位（如1998年，y=98；2001年，y=1）；
;     d为某月内的日数。
;如果w值为负数，需要修正。

#lang racket

(require "julian_date.rkt"
         "daies_between_date.rkt"
         "leap_year.rkt")

(provide
 (contract-out
  [zeller-week (-> year/c month/c day/c any)]))

;用蔡勒公式计算星期几：
(define (zeller-week year month day)
  (let* ([year1 (amend-year year month)]
         [c (quotient year1 100)]
         [y (remainder year1 100)]
         [m (amend-month month)]
         [d day]
         [w (remainder
             (ceiling
              (if (gregorian-date? year month day)
                  (+ y (/ y 4) (/ c 4) (- (* 2 c))
                     (/ (* 13 (+ m 1)) 5)
                     d (- 1))
                  (+ 5 (- c) y (/ y 4)
                     (/ (* 13 (+ m 1)) 5)
                     d (- 1))))
             7)])
    ;修正w值：
    (amend-w w)))

;修正month：
(define (amend-month month)
  (if (<= month 2)
      (+ month 12)
      month))

;根据月份相应修正年份：
(define (amend-year year month)
  (if (<= month 2)
      (- year 1)
      year))

;修正w值：
(define (amend-w w)
  (if (< w 0)
      (+ w 7)
      w))