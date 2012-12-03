;Charles Treatman
;Lab 6
(require (lib "compat.ss"))
(require (lib "eopl.ss" "eopl"))

;Exercise 2
(define list-of-ex-2?
  (lambda (lat)
    (andmap ex-2? lat)))

(define-datatype ex-2 ex-2?
                 (lit-exp 
                  (datum number?))
                 (var-exp
                  (id symbol?))
                 (lambda-exp
                  (ids list?)
                  (body ex-2?))
                 (app-exp
                  (rator ex-2?)
                  (rands list-of-ex-2?))
                 (if-exp
                  (test-exp ex-2?)
                  (then-exp ex-2?)
                  (else-exp ex-2?)))
(define parse-2
  (lambda (datum)
    (cond
      ((number? datum) (lit-exp datum))
      ((symbol? datum) (var-exp datum))
      ((pair? datum) (if (eqv? (car datum) 'if)
           (if-exp
            (parse-2 (cadr datum))
            (parse-2 (caddr datum))
            (parse-2 (cadddr datum)))
           (if (eqv? (car datum) 'lambda)
           (lambda-exp (cadr datum)
                       (parse-2 (caddr datum)))
           (app-exp
            (parse-2 (car datum))
            (map parse-2 (cdr datum))))))
       (else (error 'parse-2
             	   "Invalid concrete syntax ~s" datum)))))

(define unparse-2
  (lambda (exp)
    (cases ex-2 exp
           (lit-exp (datum) datum)
           (var-exp (id) id)
           (lambda-exp (ids body) 
                       (list 'lambda ids
                             (unparse-2 body)))
           (app-exp (rator rands)
                    (cons (unparse-2 rator)
                          (map unparse-2 rands)))
           (if-exp (test-exp then-exp else-exp)
                   (list 'if (unparse-2 test-exp)
                         (unparse-2 then-exp)
                         (unparse-2 else-exp))))))


