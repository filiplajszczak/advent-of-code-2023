#!/usr/bin/guile -s
!#

(use-modules
             ((srfi srfi-1) #:select (first last fold)
             ((srfi srfi-64) #:select (test-begin
                                       test-end
                                       test-equal))
             ((f) #:select (read-lines))
             ((algorithms) #:select (all? sum)))


(define (input filename)
   (read-lines filename))

(define (split-to-pair s)
  (let ([two (string-split (string-trim s) #\space)])
    (cons (last two)
          (string->number (first two)))))

(define (parse-line line)
  (let ([parts (string-split line #\:)])
    (cons (string->number (last (string-split (first parts) #\space)))
          (map
            (λ (draw) (map
                        (λ (splitted-draw) (split-to-pair splitted-draw))
                        (string-split draw #\,)))
            (string-split (last parts) #\;)))))

(define (reveal-possible? reveal)
  (let ([contained '(("green" . 13) ("blue" . 14) ("red" . 12))])
    (all? (map (λ (pair) (<= (cdr pair) (assoc-ref contained (car pair))))
               reveal))))

(define (add-possible game acc)
  (if (all? (map reveal-possible? (cdr game)))
      (+ acc (car game))
      acc))

(define (update reveal acc)
  (fold (λ (pair inner-acc)
  (if (> (cdr pair) (assoc-ref inner-acc (car pair)))
      (assoc-set! inner-acc (car pair) (cdr pair))
      inner-acc)) acc reveal))

(define (power game)
  (let ([minimum (fold update (list (cons "green" 0) (cons "blue" 0) (cons "red" 0)) (cdr game))])
       (* (assoc-ref minimum "green") (assoc-ref minimum "blue") (assoc-ref minimum "red"))))

(define (part-1 filename)
        (fold add-possible 0 (map parse-line (input filename))))

(define (part-2 filename)
        (sum (map power (map parse-line (input filename)))))

;; Part 1
(display (part-1 "day02.in"))
(newline)
;; Part 2
(display (part-2 "day02.in"))
(newline)

;; Tests
(test-begin "example")
;
(test-equal "Test part 1" 8 (part-1 "day02-example.in"))
(test-equal "Test part 2" 2286 (part-2 "day02-example.in"))
;
(test-end "example")
