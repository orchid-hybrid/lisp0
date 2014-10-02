
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
                  (if (> n m)
                      (if (= m 0)
                          n
                          (gcd (- n m) m))
                      (gcd m n)))
                (gcd 49 21)))

