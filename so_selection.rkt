#lang racket

(define data (file->lines "stuff//g2t.txt"))

(define N (string->number (first data)))
(define list-N (build-list N values))

(define (get-item lst)
  (define (split-at: str)
    (map string->number (string-split str #rx"[:]")))
  (let* ([splitted (string-split lst)] [diez (index-of splitted "#")])
    (list (map split-at: (take splitted diez)) (drop splitted (+ 1 diez)))))

(define dataset (map get-item (rest data)))

(define (multiply-items lst)
  (map (λ(x) (list (first lst) x)) (second lst)))

(define multiplied-items (apply append (map multiply-items dataset)))

(define tags (remove-duplicates (map second multiplied-items)))

;----------------------------ENTROPY--------------------------------

(define (get-counts lst [tags tags])
  (define (class-count tag dataset)
    (length (filter (λ(x) (equal? x tag)) dataset)))
  (map (λ(x) (class-count x lst)) tags))

(define global-counts (get-counts (map second multiplied-items)))

(define (get-entropy counts)
  (define (xlogx x) (if (= 0 x) 0 (* x (log x 2))))
  (define total (apply + counts))
  (if (= 0 total)
      0
      (- (apply + (map (λ(x) (xlogx (/ x total))) counts)))))

(define global-entropy (get-entropy global-counts))

;----------------------------IG--------------------------------

(define (attach-class lst)
  (map (λ(x) (cons (second lst) x)) (first lst)))
    
(define all-words
  (apply append (map attach-class multiplied-items)))

(define (get-words index [words all-words])
  (filter (λ(x) (equal? (second x) index)) words))

(define (3split-words index)
  (define index-words (get-words index))
  (define average (/ (apply + (map third index-words)) (length index-words)))
  (define local-counts (get-counts (map first index-words)))
  (list (map - global-counts local-counts)
        (get-counts (map first (filter (λ(x) (<= (third x) average)) index-words)))
        (get-counts (map first (filter (λ(x) (> (third x) average)) index-words)))))

(define (compute-ig idx)
  (define splitted (3split-words idx))
  (define entropies (map get-entropy splitted))
  (define percentage (map (λ(x) (/ (apply + x) (length multiplied-items))) splitted))
  (apply - (cons global-entropy (map * entropies percentage))))

(define ig-words (map compute-ig list-N))

(display-lines-to-file ig-words "stuff//g2igt.txt" #:exists 'replace)