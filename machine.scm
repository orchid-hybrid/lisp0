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
    ((number?) number?)
    
    ((pair?) pair?)
    ((car) car)
    ((cdr) cdr)
    ((cons) cons)
    
    ((=) =)
    ((>) >)
    ((<) <)
    ((>=) >=)
    ((<=) <=)
    ((+) +)
    ((-) -)
    ((*) *)
    ((/) /)
    
    (else #f)))

(define (primitive-operation-arity p)
  (case p
    ((display) 1)
    ((newline) 1)

    ((eq?) 2)
    ((not) 1)
    ((number?) 1)
    
    ((pair?) 1)
    ((car) 1)
    ((cdr) 1)
    ((cons) 2)
    
    ((=) 2)
    ((>) 2)
    ((<) 2)
    ((>=) 2)
    ((<=) 2)
    ((+) 2)
    ((-) 2)
    ((*) 2)
    ((/) 2)
    
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
              ((string? thing) thing)
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
(print instruction)
              (if (pair? instruction)
                  (case (car instruction)
                    ((debug)
                     (print 'debugging)
                     (print 'stack)
                     (for-each print stack)
                     (newline)
                     (print 'registers)
                     (for-each print registers)
                     (newline)
                     (newline)
                     (loop (cdr code)))
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

(define (test4)
  (process-machine '((assign n 49)
                     (assign m 21)
                     gcd-loop
                     (branch (> n m) (label dont-swap))
                     (assign t n)
                     (assign n m)
                     (assign m t)
                     dont-swap
                     (branch (= m 0) (label done))
                     (assign n (- n m))
                     (branch #t (label gcd-loop))
                     done
                     (call (display n))
                     )))
;; #;1> (test4)
;; 7

(define (test5)
  (process-machine '((assign t (cons (cons 1 2) (cons 3 4)))    
                     (assign continue (label done))
                     (push continue)
                     (push t)
                     print-tree
                     (pop t)
                     (branch (pair? t) (label print-cons))
                     (call (display t))
                     (pop continue)
                     (branch #t continue)
                     print-cons
                     (assign first (car t))
                     (assign rest (cdr t))
                     (assign continue (label print-cdr))
                     (push rest)
                     (push continue)
                     (call (display "("))
                     (push first)
                     (branch #t (label print-tree))
                     print-cdr
                     (call (display " . "))
                     (assign continue (label print-close))
                     (pop t)
                     (push continue)
                     (push t)
                     (branch #t (label print-tree))
                     print-close
                     (call (display ")"))
                     (pop continue)
                     (branch #t continue)
                     done
                     (call (newline)))))


(define (test6)
  (process-machine '((assign x 100)
                     (assign guess 100)
                     sqrt-iter
                     (assign tmp (* guess guess))assign guess
                     (assign tmp (- tmp x))
                     (branch #t (label abs))
                     resume
                     (assign tmp (< tmp 0.001))
                     (branch tmp (label sqrt-done))
                     (assign tmp (/ x guess))
                     (assign tmp (+ guess tmp))
                     (assign tmp (/ tmp 2))
                     (assign guess tmp)
                     (branch #t (label sqrt-iter))
                     abs
                     (assign pos (< 0 tmp))
                     (branch pos (label resume))
                     (assign tmp (- tmp))
                     (branch #t (label resume))
                     sqrt-done
                     (call (display guess)))))


(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))




(define (test7)
  (process-machine '((assign k (label done))
                     (assign n 10)
                     (push k)

                     fib
                     (assign ret n)
                     (branch (< n 2) (label return))

                     (push n) ;; save register n
                     (assign n (- n 1)) ;; set n for recursive call
                     (push (label resume1)) ;; set continuation
                     (branch #t (label fib))

                     resume1
                     (pop n) ;; restore n
                     (push n) ;; save register n
                     (push ret) ;; save return value
                     (assign n (- n 2)) ;; set n for recursive call
                     (push (label resume2)) ;; set continuation
                     (branch #t (label fib))

                     resume2
                     (pop arg1) ;; resture first return val
                     (assign arg2 ret) ;; save return value
                     (pop n) ;; restore n
                     (assign ret (+ arg1 arg2)) ;; set return value
                     (branch #t (label return))
                     
                     return
                     (pop k)
                     (branch #t k)

                     done
                     (call (display ret)))))

;;

(define (ctest1)
  (process-machine
   '(

(branch #t (label g364)) foo363 (assign return-value x) (push return-value) (assign return-value x) (push return-value) (pop p2) (pop p1) (assign return-value (+ p1 p2)) (pop continue) (branch #t continue) g364 (assign return-value 7) (push return-value) (pop x) (push (label g366)) (branch #t (label foo363)) g366 (push return-value) (pop x) (push (label g365)) (branch #t (label foo363)) g365

(call (display return-value))
     )))
