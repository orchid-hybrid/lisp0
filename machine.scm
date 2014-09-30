(define (filter p list)
  (if (null? list)
      '()
      (if (p (car list))
          (cons (car list) (filter p (cdr list)))
          (filter p (cdr list)))))

(define-syntax push!
  (syntax-rules ()
    ((push! <place> <expression>)
     (let ((value <expression>))
       (set! <place> (cons value <place>))))))

(define-syntax pop!
  (syntax-rules ()
    ((pop! <place>)
     (let ((result (car <place>)))
       (set! <place> (cdr <place>))
       result))))

(define (primitive-operation? p)
  (case p
    ((display) display)
    ((newline) newline)
    
    ((eq?) eq)
    ((not) not)
    
    ((car) car)
    ((cdr) cdr)
    ((cons) cons)
    
    ((=) =)
    ((+) +)
    ((-) -)
    ((*) *)
    
    (else #f)))

(define (process-machine machine)
  (let* ((labels (filter symbol? machine))
         (label-code (map (lambda (label) (member label machine)) labels)))
    (let ((stack '())
          (registers '()))
      (define (register-value name)
        (let ((lookup (assoc name registers)))
          (if lookup
              (cdr lookup)
              (error (list "unknown register" name)))))
      (define (set-register! name value)
        (let ((lookup (assoc name registers)))
          (if lookup
              (set-cdr! lookup value)
              (push! registers (cons name value)))))
      (define (evaluate-thing thing)
        (cond ((symbol? thing) (register-value thing))
              ((number? thing) thing)
              ((boolean? thing) thing)
              ((and (pair? thing) (eq? (car thing) 'label)) thing)
              ((and (pair? thing) (primitive-operation? (car thing))) =>
               (lambda (proc)
                 (let ((values (map evaluate-thing (cdr thing))))
                   (apply proc values))))
              (else (error (list thing "not a thing!")))))
      (let loop ((code machine))
        (if (null? code)
            #f
            (let ((instruction (car code)))
              (if (pair? instruction)
                  (case (car instruction)
                    ((branch) (let ((thing (cadr instruction))
                                    (label (caddr instruction)))
                                (if (evaluate-thing thing)
                                    (let ((label* (evaluate-thing label)))
                                      (if (and (pair? label*) (eq? (car label*) 'label))
                                          (loop (cdr (assoc (cadr label*) label-code)))
                                          (error "not a label!")))
                                    (loop (cdr code)))))
                    ((assign) (let ((place (cadr instruction))
                                    (thing (caddr instruction)))
                                (set-register! place (evaluate-thing thing))
                                (loop (cdr code))))
                    ((call) (let ((thing (cadr instruction)))
                              (evaluate-thing thing)
                              (loop (cdr code))))
                    ((push) (let ((thing (cadr instruction)))
                              (push! stack (evaluate-thing thing))
                              (loop (cdr code))))
                    ((pop) (let ((place (cadr instruction)))
                             (set-register! place (pop! stack))
                             (loop (cdr code))))
                    (else (error "invalid instruction")))
                  (loop (cdr code)))))))))

(define (test1)
  (process-machine '(loop
                     (assign x (display 1))
                     (branch #t (label loop)))))

(define (test2)
  (process-machine '((assign n 0)
                     (assign m 1)
                     (assign l 10)
                     loop
                     (call (display n))
                     (call (newline))
                     (assign tmp n)
                     (assign n (+ m n))
                     (assign m tmp)
                     (assign l (- l 1))
                     (branch (not (= l 0)) (label loop))
                     )))
;; #;1> (test2)
;; 0
;; 1
;; 1
;; 2
;; 3
;; 5
;; 8
;; 13
;; 21
;; 34
;; #f

(define (test3)
  ;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-31.html#%_sec_5.1
  (process-machine '((assign n 5)
                     (assign continue (label fact-done))
                     fact-loop
                     (branch (= n 1) (label base-case))
                     (push continue)
                     (push n)
                     (assign n (- n 1))
                     (assign continue (label after-fact))
                     (branch #t (label fact-loop))
                     after-fact
                     (pop n)
                     (pop continue)
                     (assign val (* n val))
                     (branch #t continue)
                     base-case
                     (assign val 1)
                     (branch #t continue)
                     fact-done
                     (call (display val))
                     )))
;; #;1> (test3)
;; 120
