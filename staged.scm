
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
           (testk (lambda (r e) `(if r
                                   ,(passk '() e)
                                   ,(failk '() e)))))
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
  (fold-args-aux args k))

(define (lisp0-abs program)
  (lisp0ss-collect-defs program)
  (eval0 (cons 'begin program)
         '()
         (lambda (r e) (display `((ret-val . ,r) (env . ,e))))))
