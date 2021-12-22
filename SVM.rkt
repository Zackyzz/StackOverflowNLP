#lang racket

(define trainset (file->lines "so//g2_15_train.txt"))
(define testset (file->lines "so//g2_15_test.txt"))

(define dimension (string->number (first trainset)))

(define (list-hash lst)
  (define hashlst (make-hash))
  (define sum1 (apply + (map second lst)))
  (for ([i lst])
    (hash-set! hashlst (first i) (/ (second i) sum1)))
  hashlst)

(define (get-item lst)
  (define (split-at: str)
    (map string->number (string-split str #rx"[:]")))
  (let* ([splitted (string-split lst)] [diez (index-of splitted "#")])
    (list (first (drop splitted (+ 1 diez))) (list-hash (map split-at: (take splitted diez))))))

(define (get-dataset fileset)
  (for/list ([i fileset])
    (get-item i)))

(define pre-train-set (get-dataset (rest trainset)))
(define pre-test-set (get-dataset (rest testset)))

(define temp-classes (remove-duplicates (map first pre-train-set)))

(define (rename-class lst [classes temp-classes])
  (list (index-of temp-classes (first lst)) (second lst)))

(define classes (build-list (length temp-classes) values))

(define train-set (map rename-class pre-train-set))
(define test-set (map rename-class pre-test-set))

;--------------------------------------------------------------------------

(define (change-class cls dataset)
  (map (λ(x) (if (= cls (first x)) (cons 1 (rest x)) (cons -1 (rest x)))) dataset))

(define (vectorize set)
  (for/vector ([i set])
    (vector 0 i)))

(define smo-sets
  (for/vector ([i classes])
    (vectorize (change-class i train-set))))

(define (dot2 x y [f sqr])
  (f (for/fold ([sum 0])
               ([(k v) (in-hash x)])
       (+ sum (* (hash-ref y k 0) v)))))

(define (dot u v)
  (exp (* -1
          (let ((indices (remove-duplicates (append (hash-keys u) (hash-keys v)))))
            (apply + (map (lambda (i) (let ((x (hash-ref u i 0))
                                            (y (hash-ref v i 0)))
                                        (sqr (- x y))))
                          indices))))))

(define (random-j i N)
  (define rnd (random 0 N))
  (if (not (= rnd i))
      rnd
      (random-j i N)))

(define (vfirst vect)
  (vector-ref vect 0))

(define (vsecond vect)
  (vector-ref vect 1))

(define (f set X [bias b])
  (for/fold ([sum bias])
            ([i set])
    (define xy (vsecond i))
    (+ sum (* (vfirst i) (first xy) (dot X (second xy))))))

(define N (length train-set))
(define C 5)
(define tol 0.01)
(define b 0)
(define bs
  (for/vector ([i classes])
    0))

(define simplifier empty)
(define smo-set #())

(define (multiclass-SMO)
  (for ([i classes])
    (printf "Class: ~a\n" i)
    (set! smo-set (vector-ref smo-sets (index-of classes i)))
    (set! b (vector-ref bs (index-of classes i)))
    (define max-passes 0)
    (define (main [num-changed 0] [examine-all 1] [iteration 0])
      (printf "Iteration: ~a ~a Changed: ~a \n"
              iteration (vector-length smo-set) num-changed)
      (cond
        [(> max-passes 0) iteration]
        ;[(and (> iteration 1) (= num-changed 0)) iteration]
        [(not (or (= examine-all 1) (> num-changed 0))) iteration]
        [else
         (set! N (vector-length smo-set))
         (set! num-changed 0)
         (if (= 1 examine-all)
             (for ([i N])
               (set! num-changed (+ num-changed (examine-example i))))
             (for ([i N])
               (define alphai (vfirst (vector-ref smo-set i)))
               (when (and (> alphai 0) (< alphai C))
                 (set! num-changed (+ num-changed (examine-example i))))))

         (define temp empty)
         (for ([i smo-set])
           (unless (= (vfirst i) 0)
             (set! temp (cons i temp))))
         (set! smo-set (list->vector temp))
         
         (when (= num-changed 0)
           (set! max-passes (+ max-passes 1)))
         (if (= 1 examine-all)
             (set! examine-all 0)
             (when (= 0 num-changed)
               (set! examine-all 1)))
         (main num-changed examine-all (+ 1 iteration))]))

    (time (main))
  
    (vector-set! smo-sets (index-of classes i) smo-set)
    (vector-set! bs (index-of classes i) b)
    (set! simplifier empty)))

(define (examine-example i)
  (define done 0)
  (define alphai (vfirst (vector-ref smo-set i)))
  (define xyi (vsecond (vector-ref smo-set i)))
  (define xi (second xyi))
  (define yi (first xyi))
  (define Ei (- (f simplifier xi b) yi))

  (when (or (and (< (* Ei yi) (- tol)) (< alphai C))
            (and (> (* Ei yi) tol) (> alphai 0)))

    (define non-bound empty)
    (for ([i smo-set] [index N])
      (unless (or (= (vfirst i) 0) (= (vfirst i) C))
        (set! non-bound (cons (list index i) non-bound))))
    
    (when (= done 0)
      (let loop ([clone (shuffle non-bound)])
        (cond
          [(or (equal? empty clone) (= done 1)) done]
          [else
           (define j (caar clone))
           (define setj (cadar clone))
           (define alphaj (vfirst setj))
           (set! done (take-step i j (list alphai xi yi Ei)))
           (loop (rest clone))])))
    (when (= done 0)
      (let loop ([j 0])
        (cond
          [(or (= j N) (= done 1)) done]
          [else
           (set! done (take-step i (random-j i N) (list alphai xi yi Ei)))
           (loop (+ 1 j))]))))
  done)

(define (take-step i j prevs)
  (define alphai (first prevs))
  (define xi (second prevs))
  (define yi (third prevs))
  (define Ei (fourth prevs))
  
  (define alphaj (vfirst (vector-ref smo-set j)))
  (define xyj (vsecond (vector-ref smo-set j)))
  (define xj (second xyj))
  (define yj (first xyj))
  (define Ej (- (f simplifier xj b) yj))

  (define L (if (= yi yj)
                (max 0 (- (+ alphai alphaj) C))
                (max 0 (- alphaj alphai))))
  (define H (if (= yi yj)
                (min C (+ alphai alphaj))
                (min C (- (+ C alphaj) alphai))))
  
  (define returner 0)
  (unless (or (= i j) (= L H))
    (define eta (- (* 2 (dot xi xj)) (dot xi xi) (dot xj xj)))
    (unless (>= eta 0)
      (define aj (- alphaj (/ (* yj (- Ei Ej)) eta)))
      (when (> aj H)
        (set! aj H))
      (when (< aj L)
        (set! aj L))

      (unless (< (abs (- alphaj aj)) tol)
        (define ai (+ alphai (* yi yj (- alphaj aj))))
        (vector-set! smo-set i (vector ai (list yi xi)))
        (vector-set! smo-set j (vector aj xyj))

        (set! simplifier empty)
        (for ([i smo-set])
          (unless (= (vfirst i) 0)
            (set! simplifier (cons i simplifier))))
        (set! returner 1)

        (define b1 (- b Ei (* yi (- ai alphai) (dot xi xi))
                      (* yj (- aj alphaj) (dot xi xj))))
        (define b2 (- b Ej (* yi (- ai alphai) (dot xi xj))
                      (* yj (- aj alphaj) (dot xj xj))))
        (if (and (> ai 0) (< ai C))
            (set! b b1)
            (if (and (> aj 0) (< aj C))
                (set! b b2)
                (set! b (/ (+ b1 b2) 2)))))))
  returner)

(time (multiclass-SMO))

(define (update-matrix r p matrix)
  (define (index r p i)
    (cond
      [(= r p i) 0]
      [(and (= r i) (not (= r p))) 1]
      [(and (= p i) (not (= r p))) 2]
      [else 3]))
  (for/list ([classX matrix] [i classes])
    (list-update classX (index r p i) add1)))

(define (test-svm test-set)
  (let loop ([clone test-set]
             [error-matrix (make-list (length classes) (make-list 4 0))]
             [corrects 0])
    (cond
      [(equal? empty clone) (list error-matrix corrects)]
      [else
       (define reality (caar clone))
       (define element (cadar clone))

       (define prediction
         (caar (sort
                (for/list ([i classes])
                  (list i (f (vector-ref smo-sets (index-of classes i))
                             element (vector-ref bs (index-of classes i)))))
                > #:key (λ(x) (second x)))))
       
       (loop (rest clone) (update-matrix reality prediction error-matrix)
             (if (= reality prediction) (+ 1 corrects) corrects))])))

(define results (test-svm test-set))
(define error-matrix (first results))

(define (cut-digits n)
  (exact->inexact (/ (floor (* n (expt 10 4))) (expt 10 2))))

(define confusion-matrix
  (for/list ([i error-matrix] [j temp-classes])
    (define TP (first i)) ;r p
    (define FN (second i)) ;r !p
    (define FP (third i)) ;!r p
    (define TN (fourth i)) ;!r !p

    (define Accuracy (/ (+ TP TN) (+ TP FN FP TN)))
    (define Precision (/ TP (if (= 0 (+ TP FP)) 1 (+ TP FP))))
    (define Recall (/ TP (if (= 0 (+ TP FP)) 1 (+ TP FN))))

    (printf "Class ~a:\n" j)
    (printf "Accuracy: ~a%\n" (cut-digits Accuracy))
    (printf "Precision: ~a%\n" (cut-digits Precision))
    (printf "Recall: ~a%\n" (cut-digits Recall))
    (printf "Error matrix: ~a \n\n" i)
    (map exact->inexact (list Accuracy Precision Recall))))

(map cut-digits (map (λ(x) (/ x (length classes))) (apply map + confusion-matrix)))
(printf "Global accuracy: ~a%"
        (cut-digits (exact->inexact (/ (second results) (length test-set)))))