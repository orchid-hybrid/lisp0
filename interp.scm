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
  (cddr exp))
;; (definition-body '(define (x y z) b ...)) => (b ...)

;; exp -> env -> ((maybe exp) . env)
(define (lisp0-collect-def exp)
  (cond
   ;; (define (x y z) b ...) => (cons (x 
   ((definition? exp) (cons (list)
                            (list (cons (definition-name exp) exp))))
   (else (cons (list exp) (list)))))

(define (lisp0-collect-defs program prog-env)
  (if (null? program)
      prog-env
      (let ((exp-env (lisp0-collect-def (car program))))
        (let ((exp (car exp-env))
              (env (cdr exp-env))
              (prog (car prog-env))
              (penv (cdr prog-env)))
          (lisp0-collect-defs (cdr program)
                              (cons (append exp prog) (append env penv)))))))

(define (let-exp? exp)
  (and (list? exp)
       (eql? 'let (car exp))))

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

(define (lisp0-eval-begin exps)
  (if (null? exps)
      '()
      (begin (lisp0-eval (car exps))
             (lisp0-eval-begin (cdr exps)))))

(define (prims)
  `(list (cons 'car car)
         (cons 'cdr cdr)
         (cons 'cons cons)
         (cons '= =)
         (cons '+ +)
         (cons '- -)
         (cons '* *)
         (cons '> >)
         (cons '< <)
         (cons '>= >=)
         (cons '<= <=)
         (cons 'eq? eq?)
         (cons 'display display)
         (cons 'read read)))

(define (quote-exp? exp)
  (and (list? exp) (eq? 'quote (car exp))))

(define (lisp0-eval exp env)
  (cond
   ((number? exp) exp)
   ((boolean? exp) exp)
   ((symbol? exp) (cadr (assoc exp env)))
   ((quote-exp? exp) exp)
   ((let-exp? exp) (lisp0-eval
                    (cons 'begin (let-body exp))
                    (append (let-bindings exp) env)))
   ((begin-exp? exp)
    (lisp0-eval-begin (cadr exp)))
   ((if-exp? exp) (if (cadr exp)
                      (lisp0-eval (caddr exp) env)
                      (lisp0-eval (cadddr exp) env)))
   ((and (list? exp) (assoc (car exp) (prims)))
    (apply (assoc (car exp) (prims)) (cdr exp)))
   ((app-exp? exp)
    (let ((fn (cadr (assoc exp env))))
      (lisp0-eval
       (definition-body)
       (zip (definition-params fn) (cdr exp) '()))))))


(define (run-lisp0 program)
  (let ((exp-env (lisp0-collect-defs program)))
    (lisp0-eval
     (cons 'begin (car exp-env))
     (cdr exp-env))))
