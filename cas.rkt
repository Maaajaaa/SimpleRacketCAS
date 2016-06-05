#lang racket
(define ^ expt)
(define sqr(lambda(x) (* x x)))
(define ² sqr)
(define ln log)
(define e (exp 1))

;Evaluation von Ausdrücken
(define superelement?
  (lambda (el ls)
    (cond
      [(null? ls) #f]
      [(list? (car ls)) (or (superelement? el (car ls)) (superelement? el (cdr ls)))]
      [(equal? el (car ls)) #t]
      [else
       (superelement? el (cdr ls))])))

(define berechne
  (lambda (term var wert)
    (cond 
      [(number? term) term]
      [(equal? 'Unbekannt! term) 'Unbekannt!]
      [(superelement? 'Unbekannt! term) 'Unbekannt!]
      [else
       ((eval (list 'lambda (list var) term)) wert)])))

;Differentiationsregeln
(define ableitung
  (lambda (ausdr var)
    (cond
      [(number? ausdr) 0]
      [(symbol? ausdr) (if (eqv? ausdr var) 1 0)]
      [(= (length ausdr) 2)
       (let ([op (car ausdr)] [term1 (cadr ausdr)])
         (case op
           [(+) 
            (list '+ (ableitung term1 var))]
           [(-) 
            (list '- (ableitung term1 var))]
           [(sqr)
            (list '*
                  (list '/
                        1
                        (list '* 2 (ableitung term1 var)))                 
                  (ableitung term1 var))]
           [(sin)
            (list '*
                  (list 'cos term1)
                  (ableitung term1 var))]
           ;hier stehen weitere Regeln unärer Operationen
           [else
            'Unbekannt!]))]
      [(= (length ausdr) 3)
       (let ([op (car ausdr)] [term1 (cadr ausdr)] [term2 (caddr ausdr)])
         (case op
           [(+) 
            (list '+ (ableitung term1 var) (ableitung term2 var))]
           [(-) 
            (list '- (ableitung term1 var) (ableitung term2 var))]
           [(*)
            (list '+
                  (list '* (ableitung term1 var) term2)                 
                  (list '* term1 (ableitung term2 var)))]
           [(/)
            (list '/
                  (list '-
                        (list '* (ableitung term1 var) term2)                 
                        (list '* term1 (ableitung term2 var)))
                  (list '² term2))]
           ;hier stehen weitere Regeln binärer Operationen
           [else
            'Unbekannt!]))]
      [else
       'Unbekannt!])))

;Vereinfache vollständig
(define vereinfache-vollstaendig
  (lambda (ausdr)
    (oktotastischeVereinfachung ausdr) ))

;Vereinfachen bis zum Ende (oder bis der Strom ausfällt)
(define oktotastischeVereinfachung
  (lambda (ausdr)
    (if (not (eqv? ausdr (vereinfache ausdr))) (oktotastischeVereinfachung (vereinfache ausdr)) (vereinfache ausdr)) ))

;Vereinfachungsregeln
(define vereinfache
  (lambda (ausdr)
    (letrec
        ([help-unop
          (lambda (op term1)
            (case op
              [(+ *) (vereinfache term1)]
              [(/) (list '/ 1 (vereinfache term1))]
              ;hier stehen weitere Vereinfachungsregeln für unäre Operationen
             [else
              (list op (vereinfache term1))]))]
         [help-binop
          (lambda (op term1 term2)
            (case op
              [(+) (cond 
                     [(eqv? term1 0) (vereinfache term2)]
                     [(eqv? term2 0) (vereinfache term1)]
                     [(equal? term1 term2) (list '* 2 (vereinfache term1))]
                     [(and (number? term1) (number? term2)) (+ term1 term2)]
                     [else
                      (list '+ (vereinfache term1) (vereinfache term2))])]
              [(-) (cond 
                     [(eqv? term1 0) (vereinfache term2)]
                     [(eqv? term2 0) (vereinfache term1)]                     
                     [(and (number? term1) (number? term1)) (- term1 term2)]
                     [(equal? term1 term2) (0)]
                     ;[(and (number? term1) (number? term2)) (+ term1 term2)]
                     [else
                      (list '- (vereinfache term1) (vereinfache term2))])]
              [(*) (cond
                     [(eqv? term1 0) 0]
                     [(eqv? term2 0) 0]
                     [(eqv? term1 1) (vereinfache term2)]
                     [(eqv? term2 1) (vereinfache term1)]                     
                     [(and (number? term1) (number? term1)) (* term1 term2)]
                     [else
                      (list '* (vereinfache term1) (vereinfache term2))])]
              [(/) (cond
                     [(eqv? term2 1) '(vereinfache term1)]                     
                     [(and (number? term1) (number? term1)) (/ term1 term2)]
                     [else
                      (list '/ (vereinfache term1) (vereinfache term2))])]
              ;hier stehen weitere Vereinfachungsregeln für binäre Operationen
              [else
               ausdr]))]) 
      (cond
        [(or (number? ausdr) (symbol? ausdr)) ausdr]
        [(= (length ausdr) 2) (help-unop (car ausdr) (cadr ausdr))]
        [(= (length ausdr) 3)
         (help-binop (car ausdr) (cadr ausdr) (caddr ausdr))]
        [else
         ausdr]))))


  
