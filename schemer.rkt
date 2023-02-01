#lang racket
(require 2htdp/image)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))


(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a) (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ())) ; after this is proved false, no more questions to ask so we move to else
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define rember2
  (lambda (s l)
    (cond
      ((null? l) (quote ()))
      (else (cond
              ((equal? (car l) s) (cdr l))
              (else (cons (car l) (rember2 s (cdr l)))))))))

(define rember*
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((atom? (car lat))
       (cond
       ((eq? (car lat) a) (rember* a (cdr lat)))
       (else (cons (car lat) (rember* a (cdr lat))))))
      (else (cons (rember* a (car lat)) (rember* a (cdr lat)))))))

(define factorial
  (lambda (a)
    (cond
      ((<= a 0) 1)
      (else (* (factorial (- a 1)) a)))))

(define firsts
  (lambda (lat)
    (cond
       ((null? lat) (quote ()))
       (else (cons (car (car lat)) (firsts (cdr lat)))))))

(define seconds
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      ((null? (car lat)) (seconds (cdr lat)))
      (else (cons (car (cdr (car lat))) (seconds (cdr lat)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote()))
      ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
       ((null? lat) (quote()))
       ((eq? a (car lat)) (multirember a (cdr lat)))
       (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
       ((null? lat) (quote()))
       ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
       (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
       ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
       (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
       ((null? lat) (quote()))
       ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
       (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
       ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
       (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
       ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
        (else (cons (car lat) (multisubst new old (cdr lat)))))))

(define occur
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (add1 (occur a (cdr l))))
         (else (occur a (cdr l)))))
      (else (add (occur a (car l)) (occur a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
       ((eq? (car l) old) (cons new (subst* new old (cdr l))))
       (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define member*?
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) #t)
         (else (member*? a (cdr l)))))
       (else (or (member*? a (car l)) (member*? a (cdr l)))))))

(define eqlist1?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f) 
      (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f) 
      (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (l1 l2)
    (cond
      ((and (atom? l1) (atom? l2)) (eq? l1 l2))
      ((or (atom? l1) (atom? l2)) #f)
      (else (eqlist? l1 l2)))))


(define leftmost
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))




(define (square x) (* x x))

(define (cube x) (* x x x))

(define (g x) (* 3 (+ x 4)))

(define (f x) (* 3 (cube x)))

(define double(lambda (x)  (+ x x)))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define sub
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (sub n (sub1 m)))))))

(define add
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (add n (sub1 m)))))))

(define mult
  (lambda (n m)
    (cond
      (((zero? m) 0))
      (else (add n (mult n (sub1 m)))))))

(define sign?
  (lambda (n)
    (cond((> n 0) 1)((< n 0) -1)(#t 0))))

(define addtup
  (lambda (tup)
    (cond
    ((null? tup) 0)
    (else (add (car tup) (addtup (cdr tup)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) cdr tup2)
      ((null? tup2) cdr tup1)
      (else (cons (add (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define >
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (> (sub1 n) (sub1 m))))))

(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
    (cond
      ((or (< n m) (< m n)) #f)
      (else #t))))

(define ^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (mult n (^ n (sub1 m)))))))

(define div
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (div (sub n m) m))))))

; remove n from tup
(define numberrember
  (lambda (n tup)
    (cond
      ((null? tup) (quote ())) 
      ((eq? (car tup) n) (cdr tup))
      (else (cons (car tup) (numberrember n (cdr tup)))))))

; return the smallest element in a tup
(define smallest
  (lambda (tup)
    (cond
      ((eq? (cdr tup) (quote())) (car tup) )
      ((< (car tup) (car (cdr tup))) (smallest (numberrember (car (cdr tup)) tup)))
      (else (smallest (cdr tup))))))

; sort tup
(define sort
  (lambda (tup)
    (cond
      ((null? tup) (quote ()))
      (else (cons (smallest tup) (sort( numberrember (smallest tup) tup)))))))

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      ((atom? (car lat)) (add1 (length (cdr lat))))
      (else (add (length (car lat)) (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((null? lat) (quote ()))
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((null? lat) (quote ()))
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
    ((null? lat) (quote ()))
    ((number? (car lat)) (no-nums (cdr lat)))
    (else (cons (car lat) (no-nums (cdr lat)))))))

(define one?
  (lambda (num)
    (= num 1)))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) (quote +)) (and (numbered? (car (cdr (cdr aexp)))) (numbered? (car aexp))))
      ((eq? (car (cdr aexp)) (quote x)) (and (numbered? (car (cdr (cdr aexp)))) (numbered? (car aexp))))
      ((eq? (car (cdr aexp)) (quote ^)) (and (numbered? (car (cdr (cdr aexp)))) (numbered? (car aexp)))))))

(define snumbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car (cdr (cdr aexp)))) (numbered? (car aexp)))))))

(define value
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? (car (cdr aexp)) (quote +)) (add (value (car aexp)) (value (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote *)) (mult (value (car aexp)) (value (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote -)) (sub (value (car aexp)) (value (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote ^)) (^ (value (car aexp)) (value (car (cdr (cdr aexp)))))))))

(define value2
  (lambda (aexp)
     (cond
      ((atom? aexp) aexp)
      ((eq? (car aexp) (quote +)) (add (value2 (cdr aexp)) (value2 (cdr (cdr aexp)))))
      ((eq? (car aexp) (quote *)) (mult (value2 (cdr aexp)) (value2 (cdr (cdr aexp)))))
      ((eq? (car aexp) (quote -)) (sub (value2 (cdr aexp)) (value2 (cdr (cdr aexp)))))
      ((eq? (car aexp) (quote ^)) (^ (value2 (cdr aexp)) (value2 (cdr (cdr aexp))))))))
      

(define third
  (lambda (aexp)
    (car (cdr (cdr aexp)))))
