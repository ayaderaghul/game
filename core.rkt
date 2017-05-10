(require plot)
(plot-new-window? #t)

;;     t1   t2
;; s1
;; s2

;; G2: 2 player game
(define (list->matrix lst c)
  (cond
   [(empty? lst) '()]
   [else
    (cons (take lst c) (list->matrix (drop lst c)  c))]))

;; from one's perspective

;; standard hawk dove game
(define G2-HD-A (list -1 4 0 2))
;; 3 player hawk dove game
(define G3-HD-A (list -8/3 -1 0 0 -1 4 0 4/3))
;; hawk dove game variation
(define G3-HD-A1 (list -1  -1 0 0 -1 4 0 2))

;; 2 player game, 3 strategies each
;; nash demand game
(define G2-ND-A (list 2 2 2 5 5 0 8 0 0))

(define (payoff-list->matrix payoff-list no-strategies)
  (foldr
   (lambda (nxt init)
           (list->matrix init nxt))
   payoff-list no-strategies))


(define 2HDA (payoff-list->matrix G2-HD-A (list 2)))
; '((-1 4) (0 2))
(define 3HDA (payoff-list->matrix G3-HD-A (list 2 2)))
; '(((-8/3 -1) (0 0)) ((-1 4) (0 4/3)))
(define 2NDA (payoff-list->matrix G2-ND-A (list 3)))
; '((2 2 2) (5 5 0) (8 0 0))

;; transpose to other players' point of view
(define (transpose m)
  (cond
   [(empty? (car m)) '()]
   [else
    (cons (map first m) (transpose (map rest m)))]))

;; input different perspectives
;; output the payoff matrix for the game

(define (combine-2-payoff-matrices a b)
  (define bt (transpose b))
  (for/list ([i a] [j bt])
    (map cons i j)))

(define (combine-3-payoff-matrices a b c)
  (list
   (for/list
       ([i (first a)]
        [j (transpose (first b))]
        [k (first (transpose c))])
     (map list i j k))
   (for/list
       ([l (second a)]
        [m (transpose (second b))]
        [n (second (transpose c))])
     (map list l m n))))

;; best response, from one's perpective
(define (best-response a)
  (define at (transpose a))
  (define (best v)
    (define best-payoff (apply max v))
    (for/list ([i v])
      (if (= i best-payoff) (number->string i) i)))
  (map best at))

;; mark NEs
(define (mark-pure-NEs-G2 a b)
  (combine-2-payoff-matrices
   (transpose (best-response a))
   (transpose (best-response b))))

(define (mark-pure-NEs-G3 a b c)
  (combine-3-payoff-matrices
   (map transpose (map best-response a))
   (map transpose (map best-response b))
   (map transpose (map best-response c))))

;; expected payoffs

(define q-vector
  (list
   (list
    (cons 1 "q1"))
   (list
    (cons 1 1)
    (cons -1 "q1"))))

;; a constant is a constant cell: (cons 4 1)
;; a variable cell (cons -1 "q1")
;; a (row) vector of constants (list -1 4)
;; or (list (cons -1 1) (cons 4 1))
;; an element of two cells: (list (cons 1 1) (cons -1 "q1"))
;; a (column) vector of variables: q-vector
;; q-vector has two elements, one has one cell, the other has two cells

;; in general, a list of elements
;; an element is a list of cells
;; a cell is a pair

(define (do-cell-to-element f cell element)
  (cond
   [(empty? element) (cons cell element)]
   [else
    (if
     (equal? (cdr cell) (cdr (first element)))
     (cons (cons (f (car cell) (car (first element))) (cdr cell)) (rest element))
     (cons (first element) (do-cell-to-element f cell (rest element))))]))

(define (do-element-to-element f element1 element2)
  (cond
   [(empty? element1) element2]
   [else
    (do-element-to-element
     f
     (rest element1)
     (do-cell-to-element f (first element1) element2))]))

(define (add-element element1 element2)
  (do-element-to-element + element1 element2))
(define (minus-element element1 element2)
  (do-element-to-element - element1 element2))

(define (multiply-constant-to-cell c cell)
  (cons (* c (car cell)) (cdr cell)))
(define (multiply-constant-to-cells c cells)
  (for/list
      ([cell cells])
    (multiply-constant-to-cell c cell)))
(define (multiply-constant-to-element c cells)
  (multiply-constant-to-cells c cells))
(define (multiply-constant-vector-to-variable-vector constants variables)
  (map multiply-constant-to-element constants variables))

(define (expected-payoff payoffs other-probability)
  (apply add-element
         (multiply-constant-vector-to-variable-vector payoffs other-probability)))
(define (expected-payoffs payoff-matrix other-probability)
  (for/list
      ([p payoff-matrix])
    (expected-payoff  p other-probability)))
(define (expected-payoff-difference payoff-matrix other-probability)
  (apply minus-element (expected-payoffs payoff-matrix other-probability)))

(define (calculate-expectation payoff-matrix other-probability)
  (define EPs (expected-payoffs payoff-matrix other-probability))
  (cons (apply minus-element EPs)
        EPs))


;; plot

(define (get lst x)
  (cond
   [(empty? lst) #false]
   [else
    (if (equal? x (cdr (first lst)))
        (car (first lst))
        (get (rest lst) x))]))

(define (element->function elements)
  (for/list
      ([e elements]
       [i (length elements)])
    (function
     (lambda (x) (+ (get e 1) (* (get e "q1") x)))
     0 1 #:color (+ 1 i) #:label (number->string i))))

(define (plot-expectation payoff-matrix)
  (define e (calculate-expectation payoff-matrix q-vector))
  (define f (element->function e))
  (plot f #:y-min 0))



#|

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

`(plot-VR P1 (roll P1))

`(define sample
  (plot3d
   (list
    (surface3d
     (lambda (x y) (+ 2 (* -5 x) (* -5 y))) 0 1 0 1 #:color 'green)
    (surface3d
     (lambda (x y) (+ 2 (* -8 x) (* 0 y))) 0 1 0 1 #:color 'blue)
    (surface3d
     (lambda (x y) 0)))))

;; trying to plot VF of NDG (3 strategies): failing



`(plot3d (list
         (surface3d
          (lambda (x y) (+ 4 (* -5 x) (* -5 y) (* 5 x y))) 0 1 0 1 #:color 'green)
         (surface3d
          (lambda (x y) (+ 2 (* -3 x) (* -3 y) (* 3 x y))) 0 1 0 1 #:color 'blue)))

(define (EPs1 a1)
  (match-define (list pair1 pair2) a1)
  (define (helper pair)
    (match-define (list x y) pair)
    (list (cons y 1) (cons (- x y) "p2")))
  (list
   (helper pair1) (helper pair2)))



(define (EP a)
  (define (helper e)
    (match-define (list e1 e2) e)
    (define (helper1 p)
      (match-define (list (cons x y) (cons z t)) p)
      (list (cons x "p3") (cons z "p2.p3")))
    (define (helper2 p)
      (match-define (list (cons i k) (cons l m)) p)
    (list (cons i 1) (cons l "p2") (cons (- i) "p3") (cons (- l) "p2.p3")))
    (list (helper1 e1) (helper2 e2)))
  (list
   (helper (EPs1 (map first a)))
   (helper (EPs1 (map second a)))))


(define (do-pair f pair lst)
  (cond
   [(empty? lst) (cons pair lst)]
   [else
    (if
     (equal? (cdr pair) (cdr (first lst)))
     (cons (cons (f (car pair) (car (first lst))) (cdr pair)) (rest lst))
     (cons (first lst) (do-pair f pair (rest lst))))]))

(define (do-pairs f lst1 lst2)
  (cond
   [(empty? lst1) lst2]
   [else
    (do-pairs f (rest lst1) (do-pair f (first lst1) lst2))]))

(define (add-pairs lst1 lst2)
  (do-pairs + lst1 lst2))
(define (minus-pairs lst1 lst2)
  (do-pairs - lst1 lst2))

(define (EPs a)
  (define (helper e)
    (cond
     [(empty? e) '()]
     [else
      (cons
       (apply add-pairs (first e))
       (helper (rest e)))]))
  (helper (EP a)))

(define (Es1-Es2 a)
  (apply minus-pairs (EPs a)))


(define (plot-plane p)
  (plot3d
   (list
    (surface3d
     (lambda (x y)
       (+ (get p 1)
          (* (get  p "p2") x)
          (* (get p "p3") y)
          (* (get p  "p2.p3") x y)))
     0 1 0 1 #:color 'green)
    (surface3d (lambda (x y) 0)))))

(define (plot-planes lp)
  (define surfaces
    (for/list ([p lp] [c (length lp)])
      (surface3d
       (lambda (x y)
         (+ (get p 1)
            (* (get  p "p2") x)
            (* (get p "p3") y)
            (* (get p  "p2.p3") x y)))
       0 1 0 1 #:color (+ 1 c) #:label (number->string c))))
  (plot3d
   (cons (surface3d (lambda (x y) 0))
         surfaces)))







`(plot3d
 (list
  ;(surface3d
  ; (lambda (x y) (+ 4 (* -5 x) (* -5 y) (* 5 x y))) 0 1 0 1 #:color 'green #:label "h")
 ; (surface3d
  ; (lambda (x y) (+ 2 (* -2 x) (* -2 y) (* 2 x y))) 0 1 0 1 #:color 'blue #:label "d")
  (surface3d
   (lambda (x y)
     ;(if
      ;(>
     (+ 2 (* -3 x) (* -3 y) (* 3 x y))
     ;0)
      ;1
      ;0)
     )
      0 1 0 1 #:color 'green)
;  (surface3d
;   (lambda (x y)
;     (/ (- 2 (* 3 y)) (- 3 (* 3 y)))) 0 1 0 1 #:color 'blue)
  ;(surface3d
  ; (lambda (x y)
  ;   (/ (+ -2 x (*3 y))
  ;      (- (* 3 y) 3))) 0 1 0 1)
  (surface3d (lambda (x y) 0))
  ) #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1)

`(plot3d
 (list
  (surface3d
   (lambda (x y)
     (+ 8/3 (* -11/3 x) (* -11/3 y) (* 2 x y)))
   0 1 0 1 #:color 'green)
  (surface3d
   (lambda (x y)
     (+ 2 (* -3 x) (* -3 y) (* 3 x y)))
   0 1  0 1 #:color 'blue)
  (surface3d (lambda (x y) 0))
  ))


`(plot3d
 (surface3d
  (lambda (x y)
    (if (> y 1/2) 0 1)) 0 1 0 1))

`(plot
 (list
  (function
   (lambda (x)
     (/ (- 2 (* 3 x))
        (- 3 (* 3 x))))
   0 1)
  )
 #:x-max 1 #:y-max 1
 #:x-min 0 #:y-min 0)
|#
