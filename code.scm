
(define code1 '((define (loop)
                  (display 1)
                  (loop))
                (loop)))

(define code2 '((define (fib n m l)
                  (display n)
                  (newline)
                  (if (= l 0)
                      0
                      (fib (+ m n) n (- l 1))))
                (fib 0 1 10)))

(define code3 '((define (fact n)
                  (if (= n 1)
                      1
                      (* n (fact (- n 1)))))
                (display (fact 5))))

(define code4 '((define (gcd n m)
                  (if (>= n m)
                      (if (= m 0)
                          n
                          (gcd (- n m) m))
                      (gcd m n)))
                (display (gcd 49 21))))


(define code5
  '((define (print-cons c)
      (display "(")
      (print-tree (car c))
      (display " . ")
      (print-tree (cdr c))
      (display ")"))
    (define (print-num n)
      (display n))
    (define (print-tree t)
      (if (number? t)
          (print-num t)
          (print-cons t)))
    (print-tree (cons (cons 1 3) (cons 4 7)))))

(define code6
  '((define (square x) (* x x))
    (define (average x y)
      (/ (+ x y) 2))
    (define (good-enough? guess x)
      (< (abs (- (square guess) x)) 0.001))
    (define (improve guess x)
      (average guess (/ x guess)))
    (define (sqrt-iter guess x)
      (if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)   x)))
    (define (abs x)
      (if (< 0 x) x (- 0 x)))
    (define (sqrt x)
      (sqrt-iter 1.0 x))
    (display (sqrt (* 77665 77665)))))
