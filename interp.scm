(define (definition? exp)
  (and (list? exp)
       (eq? 'define (car exp))))
;; (definition? '(define (x y z) b ...)) => #t

(define (definition-name exp)
  (caadr exp))
;; (definition-name '(define (x y z) b ...)) => x

(define (definition-params exp)
  (cdadr exp))
;; (definition-params '(define (x y z) b ...) => (y z)

(define (definition-body exp)
  `(begin . ,(cddr exp)))
;; (definition-body '(define (x y z) b ...)) => (b ...)

(define (if-exp? exp)
  (and (list? exp)
       (eq? 'if (car exp))))

(define (let-exp? exp)
  (and (list? exp)
       (eq? 'let (car exp))))

(define (let-bindings exp)
  (let-bindings-aux (cadr exp) '()))

(define (let-bindings-aux bs m)
  (if (null? bs)
      m
      (let-bindings-aux
       (cdr bs)
       (cons (cons (caar bs) (cadar bs)) m))))

(define (let-body exp)
  (cddr exp))

(define (zip as bs cs)
  (if (or (null? as) (null? bs))
      cs
      (zip (cdr as) (cdr bs) (cons (list (car as) (car bs)) cs))))

(define (begin-exp? exp)
  (and (list? exp) (eq? 'begin (car exp))))

(define (lisp0-eval-begin exps env last)
  (if (null? exps)
      last
      (lisp0-eval-begin (cdr exps) env (lisp0-eval (car exps) env))))

(define (quote-exp? exp)
  (and (list? exp) (eq? 'quote (car exp))))

(define (app-exp? exp)
  (and (list? exp) ))

(define *definitions* '())

(define (lisp0-toplevel-eval exp)
  (if (definition? exp)
      (begin
        (print (list 'defining exp (list (definition-name exp)
                                        (definition-params exp)
                                        (definition-body exp))))
        (set! *definitions* (cons (list (definition-name exp)
                                        (definition-params exp)
                                        (definition-body exp))
                                  *definitions*)))
      (lisp0-eval exp '())))


(define (lisp0-eval exp env)
  ;(print exp)
  (cond
   ((number? exp) exp)
   ((boolean? exp) exp)
   ((string? exp) exp)
   ((symbol? exp) (let ((result (assoc exp env)))
                    (if result (cdr result)
                        (error "unbound variable" exp))))
   ((quote-exp? exp) exp)
   ((let-exp? exp) (lisp0-eval
                    (cons 'begin (let-body exp))
                    (append (let-bindings exp) env)))
   ((begin-exp? exp)
    (lisp0-eval-begin (cdr exp) env '()))
   ((if-exp? exp) (if (lisp0-eval (cadr exp) env)
                      (lisp0-eval (caddr exp) env)
                      (lisp0-eval (cadddr exp) env)))
   ((app-exp? exp)
    (let ((arguments (map (lambda (sub-exp) (lisp0-eval sub-exp env)) (cdr exp))))
      (cond ((primitive-operation? (car exp)) =>
             (lambda (prim)
               (apply prim arguments)))
            (else
             (let ((definition (assoc (car exp) *definitions*)))
               (if definition
                   (let ((parameters (cadr definition))
                         (body (caddr definition)))
                     (let ((extra-env (map cons parameters arguments)))
                       (lisp0-eval body extra-env)))
                   (error "no such function defined" (car exp))))))))))


(define (run-lisp0 program)
  (for-each lisp0-toplevel-eval program))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ABSTRACT MACHINE INTERP ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define *definitionsss* '())

(define (lisp0ss-collect-defs stmts)
  (cond ((null? stmts) '())
        ((definition? (car stmts))
         (set! *definitionsss*
               (cons (list (definition-name (car stmts))
                           (definition-params (car stmts))
                           (definition-body (car stmts)))
                     *definitionsss*))
         (lisp0ss-collect-defs (cdr stmts)))
        (else (lisp0ss-collect-defs (cdr stmts)))))


(define (eval0 c e k)
  (cond
   ((number? c) (k c e))
   ((boolean? c) (k c e))
   ((string? c) (k c e))
   ((symbol? c) (let ((result (assoc c e)))
                    (if result
                        (k (cdr result) e)
                        (error "unbound variable" c))))
   ((definition? c) ;; definition: skip
    (k '() e))
   ((begin-exp? c) ;; begin
    ((fold-begin (cdr c) k) '() e))

   ((if-exp? c)
    ;; if, put corresponding pass/fail clause as next step
    (let* ((condition (cadr c))
           (p (caddr c))
           (f (cadddr c))
           (passk (lambda (r e) (eval0 p e k)))
           (failk (lambda (r e) (eval0 f e k)))
           (testk (lambda (r e) (if r
                                   (passk '() e)
                                   (failk '() e)))))
      (eval0 condition e testk)))
   ((let-exp? c) ;; let
    (eval0 (cons 'begin (let-body c))
           (append (let-bindings c) e)
           (lambda (r e1) (k r e)))) ;; have to unlet values before k
   ((app-exp? c)
    ;; apply:
    ;;  - save parent env
    ;;  - run fn with fresh env
    ;;  - discard internal env of fn
    ;;  - restore parent env
    (let* ((after-call (lambda (r e1) (k r e)))
           (call (lambda (rs e)
                   (cond ((primitive-operation? (car c)) =>
                          (lambda (prim)
                            (after-call (apply prim rs) e)))
                         ((assoc (car c) *definitionsss*) =>
                          (lambda (definition)
                            (let ((parameters (cadr definition))
                                  (body (caddr definition)))
                              (eval0 body (map cons parameters rs) after-call))))
                         (else (error "no such function defined" (car c))))))
           (eval-args (fold-args (cdr c) call)))
      (eval-args '() e)))
   (else
    (error ("unknown instruction: " c)))))


(define (fold-begin-aux args k)
  (if (null? args)
      k
      (fold-begin-aux
       (cdr args)
       (lambda (r e) (eval0 (car args) e k)))))

(define (fold-begin args k)
  (fold-begin-aux (reverse args) k))

(define (fold-args-aux args k)
  (if (null? args)
      k
      (fold-args-aux
       (cdr args)
       (lambda (r e) (eval0 (car args) e (lambda (r1 e1) (k (cons r1 r) e1)))))))

(define (fold-args args k)
  (fold-args-aux (reverse args) k))

(define (lisp0-abs program)
  (lisp0ss-collect-defs program)
  (eval0 (cons 'begin program)
         '()
         (lambda (r e) (display `((ret-val . ,r) (env . ,e))))))
