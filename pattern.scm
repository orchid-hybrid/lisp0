;; patterns are as follows:
;;
;; (<symbol> <proc>* . _)
;; (<symbol> <proc>*)
;; <proc>

(define (any? _) #t)

(define (check-predicates procs exp)
  (if (null? procs)
      #t
      (and (pair? exp) ((car procs) (car exp))
           (check-predicates (cdr procs) (cdr exp)))))

(define (check-predicates-strictly procs exp)
  (if (null? procs)
      (not (null? exp))
      (and (pair? exp) ((car procs) (car exp))
           (check-predicates (cdr procs) (cdr exp)))))

(define-syntax match-pattern
  (syntax-rules (..)
    ((match-pattern <exp> (<symbol> <procs> ... ..))
     (let ((exp <exp>))
       (and (pair? exp) (equal? (car exp) '<symbol>)
            (check-predicates (list <procs> ...) (cdr exp)))))
    ((match-pattern <exp> (<symbol> <procs> ...))
     (let ((exp <exp>))
       (and (pair? exp) (equal? (car exp) '<symbol>)
            (check-predicates-strictly (list <procs> ...) (cdr exp)))))
    ((match-pattern <exp> <proc>)
     (<proc> <exp>))))

;; (define (test e)
;;   (cond ((match e symbol?) 'symbol)
;;         ((match e (+ number? number?)) 'sum)
;;         ((match e (- number? ..)) 'subtraction)
;;         ((match e (if any? any? any?)) 'if)
;;         ((match e (if any? any?)) 'sussmans-pirate)
;;         ((match e (let symbol? list? ..)) 'named-let)
;;         (else 'nothing)))

;; #;1> (test 'foo)
;; symbol
;; #;2> (test 'bar)
;; symbol
;; #;3> (test '(+ x y))
;; nothing
;; #;4> (test '(+ 3 4))
;; sum
;; #;5> (test '(if #t x y))
;; if
;; #;6> (test '(if #f y))
;; sussmans-pirate
;; #;7> (test '(let loop ((x y)) adsfsdf))
;; named-let

(define-syntax match
  (syntax-rules ()
    ((match <exp> (<pattern> <result> ...) ...)
     (let ((exp <exp>))
       (cond ((match-pattern exp <pattern>) <result> ...) ...)))))

