;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist rktlist)
  (cond [(empty? rktlist) (aunit)]
        [#t (apair (car rktlist) (racketlist->mupllist (cdr rktlist)))]))

;; pair till ~infinitiiiiie~ till aunit
(define (mupllist->racketlist mupllist)
  (cond [(aunit? mupllist) null]
        [#t (cons (apair-e1 mupllist)
                  (mupllist->racketlist (apair-e2 mupllist)))]))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(int? e) e]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(ifgreater? e) (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
                              [v2 (eval-under-env (ifgreater-e2 e) env)])
                          (if (and (int? v1) (int? v2) (> (int-num v1) (int-num v2)))
                              (eval-under-env (ifgreater-e3 e) env)
                              (eval-under-env (ifgreater-e4 e) env)))]
        [(mlet? e) (let* ([variable-name (mlet-var e)]
                          [variable-value (eval-under-env (mlet-e e) env)]
                          [eval-this (mlet-body e)])
                          (eval-under-env eval-this (cons (cons variable-name variable-value) env)))]
        [(fun? e) (closure env e)]
        [(closure? e) e]
        [(call? e) (let ([expression (eval-under-env (call-funexp e) env)]
                         [arg-val (eval-under-env (call-actual e) env)])
                     (if (closure? expression)
                         (eval-under-env (fun-body (closure-fun expression))
                                         (if (fun-nameopt (closure-fun expression))
                                             (cons (cons (fun-nameopt (closure-fun expression)) expression)
                                                   (cons (cons (fun-formal (closure-fun expression)) arg-val)
                                                         (closure-env expression)))
                                             (cons (cons (fun-formal (closure-fun expression)) arg-val)
                                                   (closure-env expression))))
                         (error "not a closure")))]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(fst? e) (let ([eval-e (eval-under-env (fst-e e) env)])
                    (if (apair? eval-e)
                        (apair-e1 eval-e)
                        (error "not a pair")))]
        [(snd? e) (let ([eval-e (eval-under-env (snd-e e) env)])
                    (if (apair? eval-e)
                        (apair-e2 eval-e)
                        (error "not a pair")))]
        [(isaunit? e) (let ([eval-e (eval-under-env (isaunit-e e) env)])
                        (if (aunit? eval-e)
                            (int 1)
                            (int 0)))]
        [(aunit? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (hybrid-mlet* lstlst e2)
  (letrec ([runitup (lambda (lstlst acc)
                                (cond [(null? lstlst) (eval-under-env e2 acc)]
                                      [#t (runitup (cdr lstlst)
                                                    (cons (cons (car (car lstlst))
                                                                (eval-under-env (cdr (car lstlst)) acc))
                                                          acc))]))])
    (runitup lstlst null)))

(define (mlet* lstlst e2)
  (cond [(null? lstlst) e2]
        [#t (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))]))

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1 (mlet "_y" e2 (ifgreater (var "_x") (var "_y")
                                         e4
                                         (ifgreater (var "_y") (var "_x") e4 e3)))))
;; Problem 4

;(define hybrid-mupl-map (lambda (fn)
;                   (letrec ([muplfn (lambda (mupllist)
;                                      (cond [(aunit? mupllist) mupllist]
;                                            [(apair (mlet (fun-formal fn) (apair-e1 mupllist) (fun-body fn))
;                                                    (muplfn (apair-e2 mupllist)))]))])
;                     muplfn)))

(define mupl-map (fun #f "to-apply"
                      (fun "partial-app" "list"
                           (ifaunit (var "list")
                                    (var "list")
                                    (apair (call (var "to-apply") (fst (var "list")))
                                           (call (var "partial-app")
                                                 (snd (var "list"))))))))



(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun "AddN" "int" (call mupl-map (fun "AddN" "x" (add (var "int") (var "x")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  "c")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
