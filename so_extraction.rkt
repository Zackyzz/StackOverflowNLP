#lang racket

(require csv-reading csv-writing (planet dyoo/porter-stemmer))

#|(define (write-csv lst path)
  (call-with-output-file path
    (lambda (output-port)
      (display-table lst output-port))
    #:exists 'replace))|#

;--------------------------DATASET-------------------------------

(define (read-csv filename)
  (call-with-input-file filename
    csv->list))

(define csv-set (rest (read-csv "stuff//g2.csv")))

(define (beautify text)
  (define cs (string-join (regexp-match* #rx"(c\\+\\+)|(c\\#)" text)))
  (string-append
   cs " "
   (string-replace
    (string-replace
     (string-replace
      (string-replace 
       (string-replace
        text
        #rx"<p>|</p>|<pre>|</pre>|<ul>|</ul>|<ol>|</ol>|<li>|</li>" " ")
       #px"<h\\d>|</h\\d>|<i>|</i>|<b>|</b>|<u>|</u>|\\d" " ")
      #rx"&(.*?);|<(.*?)>[^<]*</(.*?)>|[!%#&@$*(){},+-.\\/\":;<=>?@^_|~]|[[]|[]]" " ")
     #px"'|`" "")
    #rx"(?<=[a-z])(?=[A-Z])|[^\x1F-\x7F]+" " ")))

(define (string-repeat str [n 1])
  (string-append* (make-list n (string-append str " "))))

(define tags '("javascript" "python" "java" "c#" "c++" "php" "r" "swift" "ruby"
                            "scala" "matlab" "perl" "go" "lisp" "haskell"))

(define (remove-tags lst)
  (filter (λ(x) (member x tags)) lst))

(define (extract-information item)
  (list (map string-repeat (list (beautify (third item))))
        (list (beautify (fourth item)))
        (remove-tags (string-split (fifth item)))))

(define initial-items (map extract-information csv-set))

;------------------------CLEAN TAGS---------------------------

(define (get-frequency string)
  (map (λ(x) (list (first x) (length x)))
       (group-by (λ(x) x) (string-split string))))

(define tags-count
  (get-frequency (string-join (map string-join (map third initial-items)))))

(define relevant-tags
  (map first
       (let ([len (length initial-items)])
         (filter
          (λ(x) (and (> (second x) (* 0.05 len)) (< (second x) (* 0.95 len))))
          tags-count))))

(define (clean-tags item)
  (filter (λ(x) (member x relevant-tags)) item))

(define items
  (filter (λ(x) (not (empty? (third x))))
          (map (λ(x) (append (take x 2) (list (clean-tags (third x))))) initial-items)))

;--------------GET FREQUENY + REMOVE STOPWORDS + STEM--------------

(define important-words (string-split (file->string "so_bestwords.txt")))
(define stop-words (string-split (file->string "sw.txt")))

(define (remove-sw lst)
  (filter (λ(x) (not (member (first x) stop-words))) lst))

(define (increase-frequency lst [n 10] [keywords (map stem important-words)])
  (map (λ(x) (if (not (member (first x) keywords)) x (list (first x) (* n (second x))))) lst))

(define (stem-sw lst)
  (remove-sw (map (λ(x) (list (stem (first x)) (second x))) lst)))

(define (recalculate lst)
  (get-frequency
   (string-join
    (flatten
     (map (λ(x) (make-list (second x) (first x))) lst)))))

(define (process item)
  (list (recalculate
         (increase-frequency
          (stem-sw
           (remove-sw
            (get-frequency
             (string-append (caar item) (caadr item)))))))
        (caddr item)))

(define processed-items (map process items))

(define global-frequency
  (remove-sw
   (get-frequency
    (string-join
     (map (λ(x) (string-append (caar x) " " (caadr x))) items)))))

(define global-frequency-processed
  (recalculate
   (increase-frequency
    (stem-sw global-frequency))))

;----------------------------ENCODE--------------------------------

(define all-words (map first (sort global-frequency-processed #:key second >)))

(define (econde-item item)
  (list
   (sort
    (map (λ(x) (list (index-of all-words (first x)) (second x))) (first item))
    #:key first <)
   (second item)))

(define encoded-items (map econde-item processed-items))

(define (make-arff-format document)
  (define (get-element lst)
    (string-append (number->string (first lst)) ":" (number->string (second lst))))
  (string-append (string-join (map get-element (car document)))
                 " # " (string-join (cadr document))))

(define arff-items (map make-arff-format encoded-items))

(display-lines-to-file (cons (length all-words) arff-items) "stuff//g2inc.txt" #:exists 'replace)

;------------------------------------------------------------

(define (multiply-items lst)
  (map (λ(x) (list (first lst) x)) (second lst)))

(define multiplied-items (apply append (map multiply-items encoded-items)))

(length encoded-items)
(length multiplied-items)
(length all-words)

(for/list ([i tags])
  (list i (length (filter (λ(x) (equal? i (second x))) multiplied-items))))