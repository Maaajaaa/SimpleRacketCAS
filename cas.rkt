#lang racket
(define ^ expt)
(define sqr(lambda(x) (* x x)))
(define ² sqr)
(define ln log)
(define e (exp 1))

;English "translation"
(define derivative ableitung)
(define simplify oktotastischeVereinfachung)
(define evaluate_term berechne)

;Evaluation von termücken
(define superelement?
  (lambda (el ls)
    (cond
      [(null? ls) #f]
      [(list? (car ls)) (or (superelement? el (car ls)) (superelement? el (cdr ls)))]
      [(equal? el (car ls)) #t]
      [else
       (superelement? el (cdr ls))])))

(define berechne
  (lambda (term variable value)
    (cond
      [(number? term) term]
      [(equal? 'Unbekannt! term) 'Unbekannt!]
      [(superelement? 'Unbekannt! term) 'Unbekannt!]
      [else
       ((eval (list 'lambda (list variable) term)) value)])))

;Differentiationsregeln
(define ableitung
  (lambda (term variable)
    (cond
      [(number? term) 0]
      [(symbol? term) (if (eqv? term variable) 1 0)]
      [(= (length term) 2)
       (let ([op (car term)] [term1 (cadr term)])
         (case op
           [(+)
            (list '+ (ableitung term1 variable))]
           [(-)
            (list '- (ableitung term1 variable))]
           [(sqr)
            (list '*
                  (list '/
                        1
                        (list '* 2 (ableitung term1 variable)))
                  (ableitung term1 variable))]
           [(sin)
            (list '*
                  (list 'cos term1)
                  (ableitung term1 variable))]
           ;hier stehen weitere Regeln unärer Operationen
           [else
            'Unbekannt!]))]
      [(= (length term) 3)
       (let ([op (car term)] [term1 (cadr term)] [term2 (caddr term)])
         (case op
           [(+)
            (list '+ (ableitung term1 variable) (ableitung term2 variable))]
           [(-)
            (list '- (ableitung term1 variable) (ableitung term2 variable))]
           [(*)
            (list '+
                  (list '* (ableitung term1 variable) term2)
                  (list '* term1 (ableitung term2 variable)))]
           [(/)
            (list '/
                  (list '-
                        (list '* (ableitung term1 variable) term2)
                        (list '* term1 (ableitung term2 variable)))
                  (list '² term2))]
           ;hier stehen weitere Regeln binärer Operationen
           [else
            'Unbekannt!]))]
      [else
       'Unbekannt!])))

;simplification until no more simplification is possible
(define simplification
  (lambda (term)
    (if (not (eqv? term (sub-simplifyterm)))
        (simplification (sub-simplifyterm))
        (sub-simplifyterm)) ))

;Vereinfachungsregeln
(define sub-simplify
  (lambda (term)
    (letrec
        ([help-unop
          (lambda (op term1)
            (case op
              [(+ *) (sub-simplifyterm1)]
              [(/) (list '/ 1 (sub-simplifyterm1))]
              ;hier stehen weitere Vereinfachungsregeln für unäre Operationen
             [else
              (list op (sub-simplifyterm1))]))]
         [help-binop
          (lambda (op term1 term2)
            (case op
              [(+) (cond
                     [(eqv? term1 0) (sub-simplifyterm2)]
                     [(eqv? term2 0) (sub-simplifyterm1)]
                     [(equal? term1 term2) (list '* 2 (sub-simplifyterm1))]
                     [(and (number? term1) (number? term2)) (+ term1 term2)]
                     [else
                      (list '+ (sub-simplifyterm1) (sub-simplifyterm2))])]
              [(-) (cond
                     [(eqv? term1 0) (sub-simplifyterm2)]
                     [(eqv? term2 0) (sub-simplifyterm1)]
                     [(and (number? term1) (number? term1)) (- term1 term2)]
                     [(equal? term1 term2) (0)]
                     ;[(and (number? term1) (number? term2)) (+ term1 term2)]
                     [else
                      (list '- (sub-simplifyterm1) (sub-simplifyterm2))])]
              [(*) (cond
                     [(eqv? term1 0) 0]
                     [(eqv? term2 0) 0]
                     [(eqv? term1 1) (sub-simplifyterm2)]
                     [(eqv? term2 1) (sub-simplifyterm1)]
                     [(and (number? term1) (number? term1)) (* term1 term2)]
                     [else
                      (list '* (sub-simplifyterm1) (sub-simplifyterm2))])]
              [(/) (cond
                     [(eqv? term2 1) '(sub-simplifyterm1)]
                     [(and (number? term1) (number? term1)) (/ term1 term2)]
                     [else
                      (list '/ (sub-simplifyterm1) (sub-simplifyterm2))])]
              ;hier stehen weitere Vereinfachungsregeln für binäre Operationen
              [else
               term]))])
      (cond
        [(or (number? term) (symbol? term)) term]
        [(= (length term) 2) (help-unop (car term) (cadr term))]
        [(= (length term) 3)
         (help-binop (car term) (cadr term) (caddr term))]
        [else
         term])) ))
