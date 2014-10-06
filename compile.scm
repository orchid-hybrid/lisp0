;; 17:57

(define *machine-definitions* '())
;; (define *machine-code* '())

;; (define (emit inst)
;;   (set! *machine-code* (cons inst *machine-code*)))

;; FROM SICP

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))
(define (registers-needed s)
  (if (symbol? s) '() (car s)))
(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))
(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))
(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))
(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg)
                          (registers-needed seq1))
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving (cdr regs) seq1 seq2)))))

;;

(define (lisp0-toplevel-compile exp)
  (if (definition? exp)
      (let ((label (gensym (definition-name exp)))
            (skip (gensym)))
        (set! *machine-definitions* (cons (list (definition-name exp)
                                                label
                                                (definition-params exp))
                                          *machine-definitions*))
        (append-instruction-sequences (make-instruction-sequence
                                       '() '()
                                       (list `(branch #t (label ,skip))
                                             label))
                                      (lisp0-compile (definition-body exp))
                                      (make-instruction-sequence
                                       '() '()
                                       (list `(pop continue)
                                             `(branch #t continue)
                                             skip))))
      (lisp0-compile exp)))
  
(define (lisp0-compile-begin exps)
  (if (null? exps)
      (empty-instruction-sequence)
      (append-instruction-sequences (lisp0-compile (car exps))
                                    (lisp0-compile-begin (cdr exps)))))

;; (define (save-environment-to-stack env)
;;   (define (save name)
;;     (emit `(push ,name)))
;;   (for-each save env))

;; (define (restore-environment-from-stack env)
;;   (define (save name)
;;     (emit `(pop ,name)))
;;   (for-each restore (reverse env)))

(define (build-parameter-list n)
  (case n
    ((0) '())
    ((1) '(p1))
    ((2) '(p1 p2))
    (else (error "not that far advanced"))))

(define (lisp0-compile exp)
  ;(print exp)
  (cond
   ((number? exp) (make-instruction-sequence
                   '() '() (list `(assign return-value ,exp))))
   ((boolean? exp) (make-instruction-sequence
                    '() '() (list `(assign return-value ,exp))))
   ((string? exp) (make-instruction-sequence
                   '() '() (list `(assign return-value ,exp))))
   ((symbol? exp) (make-instruction-sequence
                   '() '() (list `(assign return-value ,exp))))
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
               (append-instruction-sequences
                (apply append-instruction-sequences
                       (map (lambda (sub-exp)
                              (append-instruction-sequences
                               (lisp0-compile sub-exp)
                               (make-instruction-sequence '() '() (list `(push return-value)))))
                            (cdr exp)))
                (apply append-instruction-sequences
                       (map (lambda (parameter)
                              (make-instruction-sequence '() '() (list `(pop ,parameter))))
                            (reverse parameter-list)))
                (make-instruction-sequence
                 '() '() (list `(assign return-value (,(car exp) . ,parameter-list))))))))
     
     
     ;; ((primitive-operation-arity (car exp)) =>
     ;;       (lambda (arity)
     ;;         (let ((parameter-list (build-parameter-list arity)))
     ;;           (for-each (lambda (sub-exp)
     ;;                       (lisp0-compile sub-exp)
     ;;                       (emit `(push return-value)))
     ;;                     (cdr exp))
     ;;           (for-each (lambda (parameter)
     ;;                       (emit `(pop ,parameter)))
     ;;                     (reverse parameter-list))
     ;;           (emit `(assign return-value (,(car exp) . ,parameter-list))))))
          
          
          (else
           
           (let ((definition (assoc (car exp) *machine-definitions*)))
             (if definition
                 (let ((label (gensym))
                       (def-label (cadr definition)))
                   (append-instruction-sequences
                    (apply append-instruction-sequences
                           (map (lambda (sub-exp)
                                  (append-instruction-sequences
                                   (lisp0-compile sub-exp)
                                   (make-instruction-sequence '() '() (list `(push return-value)))))
                                (cdr exp)))
                    (apply append-instruction-sequences
                           (map (lambda (parameter)
                                  (make-instruction-sequence '() '() (list `(pop ,parameter))))
                                (reverse (caddr definition))))
                    (make-instruction-sequence
                     '() '() (list `(push (label ,label))
                                   `(branch #t (label ,def-label))
                                   label))))
                 (error (list exp "not a definition"))))
           
           ;; (let ((definition (assoc (car exp) *machine-definitions*)))
           ;;   (if definition
           ;;       ;; compile each argument and save it on the stack
           ;;       (let ((label (gensym))
           ;;             (def-label (cadr definition)))
           ;;         (for-each (lambda (sub-exp)
           ;;                     (lisp0-compile sub-exp)
           ;;                     (emit `(push return-value)))
           ;;                   (cdr exp))
           ;;         (for-each (lambda (parameter)
           ;;                     (emit `(pop ,parameter)))
           ;;                   (reverse (caddr definition)))
           ;;         (emit `(push (label ,label)))
           ;;         (emit `(branch #t (label ,def-label)))
           ;;         (emit label))
           ;;       (error (list exp "not a definition"))))
           
           
           )))))

(define (compile-lisp0 program)
  ;;(set! *machine-code* '())
  (set! *machine-definitions* '())
  (apply append-instruction-sequences (map lisp0-toplevel-compile program)))

(define (c1)
  (compile-lisp0 '(
                   1
                   ))
  ;(for-each print (reverse *machine-code*))
  )

(define (c2)
  (compile-lisp0 '(
                   (define (foo x) x)
                   (foo 1)
                   ))
  ;(for-each print (reverse *machine-code*))
  )

(define (c3)
  (compile-lisp0 '(
                   (+ 1 3)
                   ))
  ;(for-each print (reverse *machine-code*))
  )

(define (c4)
  (compile-lisp0 '(
                   (define (foo x) (+ x x))
                   (foo (foo 7))
                   ))
  ;(for-each print (reverse *machine-code*))
  )