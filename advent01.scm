#!/usr/bin/guile -s
!#

(use-modules ((ice-9 string-fun) #:select (string-replace-substring))
             ((srfi srfi-1) #:select (first last iota fold zip))
             ((srfi srfi-64) #:select (test-begin
                                       test-end
                                       test-equal))
             ((f) #:select (read-lines))
             ((algorithms) #:select (sum zip-with)))

(define (juxt . procs)
  (λ args
    (map (λ (proc) (apply proc args)) procs)))

(define (input filename)
   (read-lines filename))

(define (part-1 filename)
  (sum
    (map (λ (l) (string->number (list->string ((juxt first last) l))))
      (map (λ (l) (filter char-numeric? l))
        (map string->list (input filename))))))

(define (numerify s)
  (let ([digits (zip-with cons (list "one" "two" "three" "four" "five" "six" "seven" "eight" "nine") (iota 9 1))])
    (map (λ (pair) (string-replace-substring s (car pair) (cdr pair))) digits)))

(define (first-match chr-lst acc)
  (cond [(char-numeric? acc) acc]
        [(null? chr-lst) acc]
        [(char-numeric? (car chr-lst)) (car chr-lst)]
        [else acc]))

(define (first-numeric proc s)
  (fold first-match #\x (map (λ (l) (filter char-numeric? l)) (apply zip (map proc (numerify s))))))

(define (part-2 filename)
  (sum
    (map (λ (l) (string->number (list->string l)))
      (map (λ (s) (list (first-numeric string->list s) (first-numeric (compose reverse string->list) s)))
        (input filename)))))

;; Part 1
(display (part-1 "day01.in"))
(newline)
;; Part 2
(display (part-2 "day01.in"))
(newline)

;; Tests
(test-begin "example")

(test-equal "Test part 1" 142 (part-1 "day01-example-1.in"))
(test-equal "Test part 2" 281 (part-2 "day01-example-2.in"))

(test-end "example")
