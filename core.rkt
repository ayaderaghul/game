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

(define (EPs1-EPs2 2eps)
  (define epst (transpose 2eps))
  (for/list ([i epst])
    (cons
     (apply - (map car i)) (cdr (first i)))))

(define (solve-inequality equation)
  (/
   (car (car equation))
   (- (car (second equation)))))

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



(define (plot-2EP equations)
  (define (make-EP equation color label)
    (define parameters (map car equation))
    (function (lambda (x) (+ (car parameters)
                             (* (second parameters) x)))
              0 1 #:color color #:label label))
  (plot (map make-EP
             equations
             (list 'blue 'red)
             (list "s1" "s2"))
        #:x-label "q" #:y-label "EP"))


(define (make-BR1 threshold color label)
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

(define (make-BR2 threshold color label)
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

(define (plot-BRs a b)
  (define EA (expected-payoffs a))
  (define EB (expected-payoffs b))
  (match-define (list (cons const1 _) (cons factor1 _)) (EPs1-EPs2 EA))
  (match-define (list (cons const2 _) (cons factor2 _)) (EPs1-EPs2 EB))
  (define threshold1 (/ const1 (- factor1)))
  (define threshold2 (/ const2 (- factor2)))
  (plot
   (list*
    (vector-field (lambda (x y)
                    (vector (+ const2 (* factor2 y))
                            (+ const1 (* factor1 x)))) #:color 'blue)
    (make-BR1 threshold1 'brown "BR1")
    (make-BR2 threshold2 'purple "BR2")
    )
    #:x-label "q" #:y-label "p"))

(define (processor f1 f2)
  (define d (sqrt (+ (sqr f1) (sqr f2))))
  (if (and (positive? f1)
           (positive? f2))
      d
      (- d)))

(define (make-fq1 a p1 p2)
  (define EA (expected-payoffs a))
  (define s1>s2 (EPs1-EPs2 (take EA 2)))
  (define s1>s3 (EPs1-EPs2 (list (first EA) (last EA))))
  (define (make-f1 p1 p2 conditions)
    (match-define (list c factor1 factor2) (map car conditions))
    (+ c (* factor1 p1) (* factor2 p2)))
  (define (f1 p1 p2) (make-f1 p1 p2 s1>s2))
  (define (f2 p1 p2) (make-f1 p1 p2 s1>s3))
  (cond
   [(> (+ p1 p2) 1) 0]
   [else
    (local
     [(define d1 (f1 p1 p2))
      (define d2 (f2 p1 p2))
      (define d (sqrt (+ (sqr d1) (sqr d2))))]
     (if (and (positive? d1)
              (positive? d2))
      d (- d)))]))


(define (plot-VR a b)
  (define (fq1 p1 p2) (make-fq1 a p1 p2))
  (define (fq2 p1 p2) (make-fq1 b p1 p2))
  (plot
   (vector-field
    (lambda (x y) (vector (fq1 x y) (fq2 x y)))
    0 1 0 1)))

(define (roll lst)
  (append (drop lst 1) (list (first lst))))

(plot-VR P1 (roll P1))

(define sample
  (plot3d
   (list
    (surface3d
     (lambda (x y) (+ 2 (* -5 x) (* -5 y))) 0 1 0 1 #:color 'green)
    (surface3d
     (lambda (x y) (+ 2 (* -8 x) (* 0 y))) 0 1 0 1 #:color 'blue)
    (surface3d
     (lambda (x y) 0)))))
