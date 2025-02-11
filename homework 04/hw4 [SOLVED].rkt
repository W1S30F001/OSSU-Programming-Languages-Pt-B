;; grade 105% !
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (cond [(> low high) empty]
        [(= low high) (cons low empty)]
        [(cons low (sequence (+ low stride) high stride))]))

(define (string-append-map los suffix)
  (map (lambda (string) (string-append string suffix)) los))

(define (list-nth-mod lox n)
  (cond [(empty? lox) (error "list-nth-mod: empty list")]
        [(> 0 n) (error "list-nth-mod: negative number")]
        [#t (car (list-tail lox (remainder n (length lox))))]))

(define ones (lambda () (cons 1 ones)))
(define (stream-for-n-steps s n)
  (cond [(= n 0) empty]
        [#t (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))]))

(define funny-number-stream (letrec ([stream-ignite (lambda (x) (cons (if (= 0 (remainder x 5))
                                                                          (- x)
                                                                          x)
                                                                      (lambda () (stream-ignite (+ x 1)))))])
                              (lambda () (stream-ignite 1))))

(define dan-then-dog (letrec ([stream-ignite (lambda (n) (cons (if (= n 0)
                                                                 (begin (set! n 1) "dan.jpg")
                                                                 (begin (set! n 0) "dog.jpg"))
                                                             (lambda () (stream-ignite n))))])
                      (lambda () (stream-ignite 0))))

(define (stream-add-zero s) (lambda () (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

;; lox loy -> stream
;; return a stream that produces a pair which first elements is from lox & second is from loy
(define (cycle-lists lox loy)
  (letrec ([len-lox (length lox)] [len-loy (length loy)]
           [cyclock (lambda (recursion-clock)
                      (cons (cons (list-ref lox (modulo recursion-clock len-lox))
                                  (list-ref loy (modulo recursion-clock len-loy)))
                            (lambda () (cyclock (+ recursion-clock 1)))))])
    (lambda () (cyclock 0))))

(define (vector-assoc v vec)
  (letrec ([range-rover (vector-length vec)]
           [scan (lambda (n) (cond [(>= n range-rover) #f]
                                   [(not (pair? (vector-ref vec n))) (scan (+ n 1))]
                                   [#t (if (equal? (car (vector-ref vec n)) v)
                                           (vector-ref vec n)
                                           (scan (+ n 1)))]))])
    (scan 0)))

(define (cached-assoc loxpairs n)
  (letrec ([cache (make-vector n #f)]
           [current-pos 0]
           [lookup (lambda (v) (let ([ans (vector-assoc v cache)])
                                 (if ans
                                     ans
                                     (let ([new-ans (assoc v loxpairs)])
                                       (if new-ans
                                           (begin (vector-set! cache (remainder current-pos n) new-ans)
                                                  (set! current-pos (+ 1 current-pos))
                                                  new-ans)
                                           #f)))))])
    lookup))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([eval-e1 e1]
              [loop (lambda ()
                      (if (>= e2 eval-e1)
                          #t
                          (loop)))])
       (loop))]))

                                           
