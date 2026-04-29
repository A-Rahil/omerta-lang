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
    
    ;; Variables: Look up the name in the ledger!
    [(list 'VarAccess name)
     (hash-ref the-ledger name 
               (lambda () (error (format "Runtime Error: Variable '~a' is not in the ledger!" name))))]
    
    ;; Math: Evaluate the left side, evaluate the right side, then do the math
    [(list 'BinOp op left right)
     (define l-val (eval-expr left))
     (define r-val (eval-expr right))
     (case op
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
    
    ;; If Statement: If the condition is not 0 (fugazzi), run the block!
    [(list 'IfStmt cond-expr true-block)
     (define condition-val (eval-expr cond-expr))
     ;; In OMERTA, 0 is false. Anything else is true.
     (unless (equal? condition-val 0)
       (for-each eval-stmt true-block))]
    
    [_ (error "Runtime Error: Unrecognized statement node.")]))

;; ==========================================
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