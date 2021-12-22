#lang racket

(define data (file->lines "stuff//g2.txt"))
(define N (string->number (first data)))
(define (part x n) (inexact->exact (floor (* n x)))) 
(define M (part N 0.15))
(define list-N (build-list N values))
(define igs (sort (map list list-N (file->list "stuff//g2igt.txt")) #:key second >))

(define (get-item lst)
  (define (split-at: str)
    (map string->number (string-split str #rx"[:]")))
  (let* ([splitted (string-split lst)] [diez (index-of splitted "#")])
    (list (map split-at: (take splitted diez)) (drop splitted (+ 1 diez)))))

(define dataset (map get-item (rest data)))

;----------------------SELECT BEST-WORDS-------------------

(define best-words (take (map first igs) M))

(define (select-words item)
  (list (sort
         (map (λ(x) (list (index-of best-words (first x)) (second x)))
              (filter (λ(x) (member (first x) best-words)) (first item)))
         #:key first <)
        (second item)))

(define featured-items
  (filter (λ(x) (not (empty? (first x))))
          (map select-words dataset)))

(define (make-arff-format item)
  (define (get-element lst)
    (string-append (number->string (first lst)) ":" (number->string (second lst))))
  (string-append (string-join (map get-element (car item)))
                 " # " (string-join (cadr item))))

(define arff-items (shuffle (map make-arff-format featured-items)))

(display-lines-to-file
 (cons M (take arff-items (part (length arff-items) 0.7))) "stuff//g2t_15_train.txt" #:exists 'replace)

(display-lines-to-file
 (cons M (drop arff-items (part (length arff-items) 0.7))) "stuff//g2t_15_test.txt" #:exists 'replace)

;----------------------NORMALIZATION--------------------

(define (sum1 item)
  (define n-dt (apply + (map second (first item))))
  (list
   (map (λ(x) (list (first x) (exact->inexact (/ (second x) n-dt)))) (first item))
   (second item)))

(define normalized-items (map sum1 featured-items))