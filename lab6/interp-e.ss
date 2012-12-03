;; interp-e.ss
;;   Interpreter for Mini-Scheme-E

(require (lib "eopl.ss" "eopl"))

(define eval-exp
   (lambda (exp)
      (cases exp-e exp
        (exp-e-lit (datum) datum)
        (exp-e-varref (var) (apply-env init-env var))
	(exp-e-app (rator rands)
           (let ([proc (eval-exp rator)] [args (eval-rands rands)])
              (apply-proc proc args)))
        (else (error 'eval-exp
                  "Invalid abstract syntax: ~s"
                  exp)))))

(define eval-rands (lambda (rands) (map eval-exp rands)))

(define apply-prim-op
   (lambda (prim-op args)
      (case prim-op
        [(+) (+ (car args) (cadr args))]
        [(-) (- (car args) (cadr args))]
        [(*) (* (car args) (cadr args))]
        [(add1) (+ (car args) 1)]
        [(sub1) (- (car args) 1)]
        [(minus) (- 0 (car args))]
        [(list) (cons (car args) (cdr args))]
        [(build) (cons (car args) (cadr args))]
        [(first) (caar args)]
        [(rest) (cdar args)]
        [else
          (error 'apply-prim-op
             "Invalid prim-op name: ~s"
             prim-op)])))

; primop definitions (same as interp-b)

(define-datatype prim prim?
  (prim-proc (prim-op proc-symbol?)))

(define proc-symbol? (lambda (sym) (member sym '(+ - * / add1 sub1 minus
                                                   list build first rest))))

(define prim-op-names '(+ - * add1 sub1 minus list build first rest))

(define init-env
  (extend-env '(nil) '(())
              (extend-env prim-op-names (map prim-proc prim-op-names)
                          the-empty-env)))

; apply-proc

(define apply-proc
  (lambda (proc args)
    (cases prim proc
	(prim-proc (prim-op) (apply-prim-op prim-op args))
	(else (error 'apply-proc "Invalid procedure: ~s" proc)))))