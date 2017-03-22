;;     t1   t2
;; s1
;; s2

(define (payoff lst r c)
  (cond
   [(empty? lst) '()]
   [else
    (cons (take lst c) (payoff (drop lst c) r c))]))

(define A (payoff (list 1 3 4 0) 2 2))
(define B (payoff (list -1 2 0 -1) 2 2))

(define (transpose m)
  (cond
   [(empty? (car m)) '()]
   [else
    (cons (map first m) (transpose (map rest m)))]))

(define (payoffs a b)
  (define bt (transpose b))
  (define r (length a))
  (define c (length (first a)))
  (for/list ([i a] [j bt])
    (map cons i j)))

(define (best-response a)
  (define at (transpose a))
  (define (best v)
    (define best-payoff (apply max v))
    (for/list ([i v])
      (if (= i best-payoff) (number->string i) i)))
  (map best at))

(define (find-pure-NE a b)
  (payoffs (transpose (best-response a))
           (transpose (best-response b))))

;; nash demand game

(define P1 (payoff (list 2 2 2 5 5 0 8 0 0) 3 3))
(define P2 (payoff (list 2 2 2 5 5 0 8 0 0) 3 3))

(define (i->qi i)
  (string-append "q" (number->string i)))

(define (expected-payoffs a)
  (define (expected-payoff s1)
    (define -qs
      (rest (for/list ([i (length s1)])
              (cons -1 (i->qi i)))))
    (define (n*pair n p)
      (cons (* n (car p)) (cdr p)))
    (define -nqs
      (map (lambda (x) (n*pair (last s1) x)) -qs))
    (cons
     (cons (last s1) 1)
     (for/list ([x (drop-right s1 1)] [y -nqs])
       (cons (+ x (car y)) (cdr y)))
     ))
  (map expected-payoff a))

(require plot)
(plot-new-window? #t)

(define (plot-3EP equations)
  (define (make-surface equation color label)
    (define parameters (map car equation))
    (surface3d (lambda (x y) (+ (first parameters)
                           (* (second parameters) x)
                           (* (third parameters) y)))
               0 1 0 1 #:color color #:label label))
  (plot3d (list
           (map make-surface
                equations
                (list 'brown 'green 'pink)
                (list "s1" "s2" "s3")))
          #:x-label "q1" #:y-label "q2" #:z-label "EP"))


(define (EP equation color label)
  (define parameters (map car equation))
  (function (lambda (x) (+ (car parameters)
                      (* (second parameters) x)))
            0 1 #:color color #:label label))

(define (plot-2EP equations)
  (plot (map EP
             equations
             (list 'blue 'red)
             (list "s1" "s2"))
        #:x-label "q" #:y-label "EP"))
(define (EPs1>EPs2 2eps)
  (define epst (transpose 2eps))
  (define parameters
    (for/list ([i epst])
      (apply - (map car i))))
  (/ (car parameters) (- (second parameters))))

(define (p=1 threshold color label)
  (list
   (function
    (lambda (x) 1)
    0 threshold #:color color #:width 2)
   (function
    (lambda (x) 0)
    threshold 1 #:color color #:width 2)
   (inverse
    (lambda (x) threshold)
    0 1 #:color color #:label label #:width 2)))

(define (q=1 threshold color label)
  (list
   (inverse
    (lambda (x) 1)
    0 threshold #:color color #:width 2)
   (inverse
    (lambda (x) 0)
    threshold 1 #:color color #:width 2)
   (function
    (lambda (x) threshold)
    0 1 #:color color #:label label #:width 2)))

(define (plot-BRs equations1 equations2)
  (define threshold1 (EPs1>EPs2 equations1))
  (define threshold2 (EPs1>EPs2 equations2))
  (plot
   (list*
    (vector-field (lambda (x y) (vector (- 3 (* 4 y)) (- 1 (* 2 x)))) #:color 'blue)
    (p=1 threshold1 'brown "BR1")
    (q=1 threshold2 'purple "BR2")
    )
    #:x-label "q" #:y-label "p"))

(define ep1
  (plot
   (list
    (function-interval
     (lambda (x) (- 2/5 x))
     (lambda (x) (- -3/5 x))
     0 1)
    (inverse-interval
     (lambda (x) 1/4)
     (lambda (x) -5/4)
     0 1))
   #:x-min 0 #:x-max 1
   #:y-min 0 #:y-max 1))

(plot3d
 (list
  (surface3d (lambda (x y) (- 3/20 y)) 0 1 0 1 #:color 'green)
  (surface3d (lambda (x y) (- 1 (* 4 x))) 0 1 0 1 #:color 'blue))
 #:z-min 0 #:z-max 1)

(define (processor f1 f2)
  (define d (sqrt (+ (sqr f1) (sqr f2))))
  (if
   (and (positive? f1) (positive? f2))
   d
   (- d)))
(define (fq1 p1 p2)
  (define (f1 p1 p2) (- 3/20 p2))
  (define (f2 p1 p2) (- 1 (* 4 p1)))
  (processor (f1 p1 p2) (f2 p1 p2)))
(define (fq2 p1 p2)
  (define (f1 p1 p2) (- p2 (* 3/5 p1)))
  (define (f2 p1 p2) (+ p1 p2 -2/5))
  (processor (f1 p1 p2) (f2 p1 p2)))

(define (plot-VR)
  (plot
   (vector-field
    (lambda (x y) (vector (fq1 x y) (fq2 x y)))
    0 1 0 1)))
(plot-VR)
