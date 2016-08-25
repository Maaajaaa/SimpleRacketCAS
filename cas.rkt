#lang racket
(define ^ expt)
(define ² sqr)
(define ln log)
(define e (exp 1))

;derivation
(define derivative
  (lambda (term variable)
    (cond
      [(number? term) 0]
      [(symbol? term) (if (eqv? term variable) 1 0)]
      [(= (length term) 2)
       (let ([op (car term)] [term1 (cadr term)])
         (case op
           ;derivation of unary operations (only one term)
           [(+)
            (list '+ (derivative term1 variable))]
           [(-)
            (list '- (derivative term1 variable))]
           ;sqr(x) == x*x
           [(sqr)
            (derivative (list '* term1 term1) variable)]
           ;sqrt(x) == x^(1/2)
           [(sqrt)
            (derivative (list '^ term1 (/ 1 2)) variable)]
           [(sin)
            (list 'cos term1)]
           [(cos)
            (list '-sin term1)]
           ;tan(x) == sin(x)/cos(x)
           [(tan)
            (derivative (list '/ (list 'sin term1) (list 'cos term1)) variable)]
           [(ln)
            (derivative (list 'log term1 'e) variable)]
           [else
            'Unknown!]))]
      [(= (length term) 3)
       (let ([op (car term)] [term1 (cadr term)] [term2 (caddr term)])
         (case op
           ;derivation of binary operations (two terms)
           [(+)
            (list '+ (derivative term1 variable) (derivative term2 variable))]
           [(-)
            (list '- (derivative term1 variable) (derivative term2 variable))]
           [(*)
            (list '+
                  (list '* (derivative term1 variable) term2)
                  (list '* term1 (derivative term2 variable)))]
           [(/)
            (derivative (list '* term1 (list '^ term2 (- 1))) variable)]
           [(log)
            (list '/ 1 (list '* term1 (list 'ln term2)))]
           ;TODO: add ^
           [else
            'Unknown!]))]
      [else
       'Unknown!])))

;simplification of the term until no more simplification is possible
(define simplification
  (lambda (term)
    (if (not (eqv? term (sub-simplify term)))
        (simplification (sub-simplify term))
        (sub-simplify term)) ))

;simplification of one term (not the result of this simplification)
(define sub-simplify
  (lambda (term)
    (letrec
        ([help-unop
          (lambda (op term1)
            (case op
              ;simplification of unary operations (only one term)
              [(+ *) (sub-simplify term1)]
              [(/) (list '/ 1 (sub-simplify term1))]
             [else
              (list op (sub-simplify term1))]))]
         [help-binop
          (lambda (op term1 term2)
            (case op
              ;simplification of binary operations (two terms)
              [(+) (cond
                     [(eqv? term1 0) (sub-simplify term2)]
                     [(eqv? term2 0) (sub-simplify term1)]
                     [(equal? term1 term2) (list '* 2 (sub-simplify term1))]
                     [(and (number? term1) (number? term2)) (+ term1 term2)]
                     [else
                      (list '+ (sub-simplify term1) (sub-simplify term2))])]
              [(-) (cond
                     [(eqv? term1 0) (sub-simplify term2)]
                     [(eqv? term2 0) (sub-simplify term1)]
                     [(and (number? term1) (number? term1)) (- term1 term2)]
                     [(equal? term1 term2) (0)]
                     [else
                      (list '- (sub-simplify term1) (sub-simplify term2))])]
              [(*) (cond
                     [(eqv? term1 0) 0]
                     [(eqv? term2 0) 0]
                     [(eqv? term1 1) (sub-simplify term2)]
                     [(eqv? term2 1) (sub-simplify term1)]
                     [(and (number? term1) (number? term1)) (* term1 term2)]
                     [else
                      (list '* (sub-simplify term1) (sub-simplify term2))])]
              [(/) (cond
                     [(eqv? term2 1) '(sub-simplify term1)]
                     [(and (number? term1) (number? term1)) (/ term1 term2)]
                     [else
                      (list '/ (sub-simplify term1) (sub-simplify term2))])]
              [else
               term]))])
      (cond
        [(or (number? term) (symbol? term)) term]
        [(= (length term) 2) (help-unop (car term) (cadr term))]
        [(= (length term) 3)
         (help-binop (car term) (cadr term) (caddr term))]
        [else
         term])) ))
