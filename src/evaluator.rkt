#lang racket/base

(require racket/match)

(provide evaluate)

;; THE LEDGER (Our Symbol Table for Variables)
;; This hash table stores all our variable names and their values.
(define the-ledger (make-hash))

;; ==========================================
;; 1. EVALUATE EXPRESSIONS (Math and Values)
;; ==========================================
(define (eval-expr expr-node)
  (match expr-node
    ;; Base values: just return the raw data
    [(list 'NumLiteral val) val]
    [(list 'StringLiteral str) str]
    [(list 'BoolLiteral val) val]
    
    ;; Variables: Look up the name in the ledger!
    [(list 'VarAccess name)
     (hash-ref the-ledger name 
               (lambda () (error (format "Runtime Error: Variable '~a' is not in the ledger!" name))))]
    
    ;; Math: Evaluate the left side, evaluate the right side, then do the math
    [(list 'BinOp op left right)
     (define l-val (eval-expr left))
     (define r-val (eval-expr right))
     (case op
       ['== (equal? l-val r-val)]
       ['!= (not (equal? l-val r-val))]
       ['>  (> l-val r-val)]
       ['<  (< l-val r-val)]
       ['>= (>= l-val r-val)]
       ['<= (<= l-val r-val)]
       ['+ (+ l-val r-val)]
       ['- (- l-val r-val)]
       ['* (* l-val r-val)]
       ['/ (/ l-val r-val)])]
    
    [_ (error "Runtime Error: Unrecognized expression node.")]))

;; ==========================================
;; 2. EVALUATE STATEMENTS (Actions)
;; ==========================================
(define (eval-stmt stmt-node)
  (match stmt-node
    
    ;; Variable Definition: Do the math, then save it in the ledger
    [(list 'VarDef type name expr)
     (define val (eval-expr expr))
     (hash-set! the-ledger name val)]
    
    ;; Print Statement: Evaluate the expression, then print it to the screen!
    [(list 'PrintStmt expr)
     (displayln (eval-expr expr))]
    
    ;; If Statement with optional yknowhat (else-if) and forgetaboutit (else)
    ;; (list 'IfStmt cond true-block elseif-list else-block)
    [(list 'IfStmt cond-expr true-block elseif-list else-block)
     (define condition-val (eval-expr cond-expr))
     (cond
       ;; Main condition is truthy
       [(and condition-val (not (equal? condition-val #f)) (not (equal? condition-val 0)))
        (for-each eval-stmt true-block)]
       ;; Check yknowhat (else-if) branches
       [(not (null? elseif-list))
        (define matched
          (for/first ([branch elseif-list]
                      #:when (let ([v (eval-expr (cadr branch))])
                               (and v (not (equal? v #f)) (not (equal? v 0)))))
            branch))
        (if matched
            (for-each eval-stmt (caddr matched))
            (for-each eval-stmt else-block))]
       ;; Fall through to forgetaboutit
       [else
        (for-each eval-stmt else-block)])]

    ;; Legacy single-branch IfStmt (backward compat)
    [(list 'IfStmt cond-expr true-block)
     (define condition-val (eval-expr cond-expr))
     (when (and condition-val (not (equal? condition-val #f)) (not (equal? condition-val 0)))
       (for-each eval-stmt true-block))]
    
    [_ (error "Runtime Error: Unrecognized statement node.")]))
    
;; =====================================
;; 3. THE MASTER RUNNER
;; ==========================================
(define (evaluate ast)
  ;; Clear the ledger every time we run a new program
  (hash-clear! the-ledger)
  
  (match ast
    [(list 'Program stmts)
     ;; Loop through every statement in the program and run it
     (for-each eval-stmt stmts)]
    
    [_ (error "Runtime Error: Invalid AST structure.")]))