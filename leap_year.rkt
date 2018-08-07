;leap_year.rkt
;计算闰年：
;格里历闰年规则：四年一闰，百年不闰，四百年再闰。

#lang racket

(provide
 (contract-out
  [leap-year? (-> year/c boolean?)]
  [year/c (-> any/c boolean?)]))

;以下定义合约检查函数--------------------------------
;检查年值：
(define (year/c year)
  (integer? year))

;以下定义计算函数------------------------------------
;判断给定的年份是否为闰年。
(define (leap-year? year)
  (if (or (and (= 0 (remainder year 4))
               (not (= 0 (remainder year 100))))
          (= 0 (remainder year 400)))
      #t
      #f))