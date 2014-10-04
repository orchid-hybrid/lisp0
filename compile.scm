(define *machine-code* '())
(define *machine-definitions* '())

(define (emit inst)
  (set! *machine-code* (cons inst *machine-code*)))

(define (lisp0-toplevel-compile exp)
  (if (definition? exp)
      (let ((label (gensym (definition-name exp)))
            (skip (gensym)))
        (set! *machine-definitions* (cons (list (definition-name exp)
                                                label
                                                (definition-params exp))
                                          *machine-definitions*))
        (emit `(branch #t (label ,skip)))
        (emit label)
        (lisp0-compile (definition-body exp))
        (emit `(pop continue))
        (emit `(branch #t continue))
        (emit skip))
      (lisp0-compile exp)))
  
(define (lisp0-compile-begin exps)
  (if (null? exps)
      #f
      (begin (lisp0-compile (car exps))
             (lisp0-compile-begin (cdr exps)))))

(define (save-environment-to-stack env)
  (define (save name)
    (emit `(push ,name)))
  (for-each save env))

(define (restore-environment-from-stack env)
  (define (save name)
    (emit `(pop ,name)))
  (for-each restore (reverse env)))

(define (build-parameter-list n)
  (case n
    ((0) '())
    ((1) '(p1))
    ((2) '(p1 p2))
    (else (error "not that far advanced"))))

(define (lisp0-compile exp)
  ;(print exp)
  (cond
   ((number? exp) (emit `(assign return-value ,exp)))
   ((boolean? exp) (emit `(assign return-value ,exp)))
   ((string? exp) (emit `(assign return-value ,exp)))
   ((symbol? exp) (emit `(assign return-value ,exp)))
   ((quote-exp? exp) (error "not implementedq"))
   ((let-exp? exp) (error "not implementedl"))
   ((begin-exp? exp)
    (lisp0-compile-begin (cdr exp)))
   ((if-exp? exp)
    (error "not imlpemented")
    (if (lisp0-compile (cadr exp))
        (lisp0-compile (caddr exp))
        (lisp0-compile (cadddr exp))))
   ((app-exp? exp)
    (cond ((primitive-operation-arity (car exp)) =>
           (lambda (arity)
             (let ((parameter-list (build-parameter-list arity)))
               (for-each (lambda (sub-exp)
                           (lisp0-compile sub-exp)
                           (emit `(push return-value)))
                         (cdr exp))
               (for-each (lambda (parameter)
                           (emit `(pop ,parameter)))
                         (reverse parameter-list))
               (emit `(assign return-value (,(car exp) . ,parameter-list)))
               )))
          (else
           (let ((definition (assoc (car exp) *machine-definitions*)))
             (if definition
                 ;; compile each argument and save it on the stack
                 (let ((label (gensym))
                       (def-label (cadr definition)))
                   (for-each (lambda (sub-exp)
                               (lisp0-compile sub-exp)
                               (emit `(push return-value)))
                             (cdr exp))
                   (for-each (lambda (parameter)
                               (emit `(pop ,parameter)))
                             (reverse (caddr definition)))
                   (emit `(push (label ,label)))
                   (emit `(branch #t (label ,def-label)))
                   (emit label))
                 (error (list exp "not a definition")))))))))

(define (compile-lisp0 program)
  (set! *machine-code* '())
  (set! *machine-definitions* '())
  (for-each lisp0-toplevel-compile program))

(define (c1)
  (compile-lisp0 '(
                   1
                   ))
  (for-each print (reverse *machine-code*)))

(define (c2)
  (compile-lisp0 '(
                   (define (foo x) x)
                   (foo 1)
                   ))
  (for-each print (reverse *machine-code*)))

(define (c3)
  (compile-lisp0 '(
                   (+ 1 3)
                   ))
  (for-each print (reverse *machine-code*)))

(define (c4)
  (compile-lisp0 '(
                   (define (foo x) (+ x x))
                   (foo (foo 7))
                   ))
  (for-each print (reverse *machine-code*)))
