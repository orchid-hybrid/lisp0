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
  
  ;(for-each print (list 'debugging regs seq1 seq2))
  ;(newline)
  
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
              (append `((push ,first-reg))
                      (statements seq1)
                      `((pop ,first-reg))))
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
                                      (lisp0-compile (definition-body exp) 'return-value)
                                      (make-instruction-sequence
                                       '() '()
                                       (list `(pop continue)
                                             `(branch #t continue)
                                             skip))))
      (lisp0-compile exp 'return-value)))
  
(define (lisp0-compile-begin exps return-value)
  (if (null? exps)
      (empty-instruction-sequence)
      (if (null? (cdr exps))
          (lisp0-compile (car exps) return-value)
          (append-instruction-sequences (lisp0-compile (car exps) 'return-value)
                                        (lisp0-compile-begin (cdr exps) return-value)))))

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

(define (lisp0-compile exp return-value)
  ;(print exp)
  (cond
   ((number? exp) (make-instruction-sequence
                   '() '() (list `(assign ,return-value ,exp))))
   ((boolean? exp) (make-instruction-sequence
                    '() '() (list `(assign ,return-value ,exp))))
   ((string? exp) (make-instruction-sequence
                   '() '() (list `(assign ,return-value ,exp))))
   ((symbol? exp) (make-instruction-sequence
                   '() '() (list `(assign ,return-value ,exp))))
   ((quote-exp? exp) (error "not implementedq"))
   ((let-exp? exp) (error "not implementedl"))
   ((begin-exp? exp)
    (lisp0-compile-begin (cdr exp) return-value))
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
                               (lisp0-compile sub-exp 'return-value)
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
                   (let loop ((exps (reverse (cdr exp)))
                              (return-value-registers (caddr definition))
                              (inner
                               (make-instruction-sequence
                                '() '() (list `(push (label ,label))
                                              `(branch #t (label ,def-label))
                                              label
                                              `(assign ,return-value return-value)))))
                     (if (null? exps)
                         inner
                         (loop (cdr exps)
                               (cdr return-value-registers)
                               (preserving (list 'return-value)
                                           (lisp0-compile (car exps) (car return-value-registers))
                                           inner)))))
                 (error (list exp "not a definition"))))

           ;; (let ((definition (assoc (car exp) *machine-definitions*)))
           ;;   (if definition
           ;;       (let ((label (gensym))
           ;;             (def-label (cadr definition)))
           ;;         (append-instruction-sequences
           ;;          (apply append-instruction-sequences
           ;;                 (map (lambda (sub-exp)
           ;;                        (append-instruction-sequences
           ;;                         (lisp0-compile sub-exp)
           ;;                         (make-instruction-sequence '() '() (list `(push return-value)))))
           ;;                      (cdr exp)))
           ;;          (apply append-instruction-sequences
           ;;                 (map (lambda (parameter)
           ;;                        (make-instruction-sequence '() '() (list `(pop ,parameter))))
           ;;                      (reverse (caddr definition))))
           ;;          (make-instruction-sequence
           ;;           '() '() (list `(push (label ,label))
           ;;                         `(branch #t (label ,def-label))
           ;;                         label))))
           ;;       (error (list exp "not a definition"))))
           
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
  (for-each print (statements
                   (compile-lisp0 '(
                                    1
                                    ))))
  ;(for-each print (reverse *machine-code*))
  )

(define (c2)
  (for-each print (statements
  (compile-lisp0 '(
                   (define (foo x) x)
                   (foo 1)
                   ))
  ;(for-each print (reverse *machine-code*))
  )
  )
  )

(define (c3)
  (for-each print (statements
  (compile-lisp0 '(
                   (+ 1 3)
                   ))
  ;(for-each print (reverse *machine-code*))
  )
  )
  )

(define (c4)
  (for-each print (statements
  (compile-lisp0 '(
                   (define (foo x) (+ x x))
                   (foo (foo 7))
                   ))
  ;(for-each print (reverse *machine-code*))
  )
  )
  )
