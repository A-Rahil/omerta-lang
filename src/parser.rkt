#lang racket/base

(require parser-tools/yacc
         "lexer.rkt")

(provide omerta-parser)

(define omerta-parser
  (parser
   ;; 1. Setup
   (start program)     ;; The top of your grammar
   (end EOF)           ;; When to stop
   (tokens value-tokens punct-tokens) ;; Import tokens from your lexer
   
   ;; 2. Error Handling
   (error (lambda (tok-ok? tok-name tok-value)
            (error (format "Syntax error! Fugazzi token found: ~a (value: ~a)" tok-name tok-value))))
   
   ;; 3. THE GRAMMAR RULES
   (grammar
    
    ;; <program> -> the-sitdown { <stmts> }
    (program
     ((THE_SITDOWN LBRACE stmts RBRACE) (list 'Program $3)))
    
    ;; <stmts> -> <stmt>! <stmts> | <stmt>!
    (stmts
     ((stmt BANG stmts) (cons $1 $3))  ;; If there are more statements, link them
     ((stmt BANG)       (list $1)))    ;; Base case: just one statement
    
    ;; <stmt> -> <var_def> | <print_stmt> | <if_stmt>
    (stmt
     ((var_def)    $1)
     ((print_stmt) $1)
     ((if_stmt)    $1))
    
    ;; <print_stmt> -> holler ( <expr> )
    (print_stmt
     ((HOLLER LPAREN expr RPAREN) (list 'PrintStmt $3)))
    
    ;; <if_stmt> -> say ( <expr> ) { <stmts> }
    (if_stmt
     ((SAY LPAREN expr RPAREN LBRACE stmts RBRACE)
      (list 'IfStmt $3 $6 '() '()))

     ((SAY LPAREN expr RPAREN LBRACE stmts RBRACE
       FORGETABOUTIT LBRACE stmts RBRACE)
      (list 'IfStmt $3 $6 '() $10))

     ((SAY LPAREN expr RPAREN LBRACE stmts RBRACE
       YKNOWHAT LPAREN expr RPAREN LBRACE stmts RBRACE)
      (list 'IfStmt $3 $6 (list (list 'ElseIf $10 $13)) '()))

     ((SAY LPAREN expr RPAREN LBRACE stmts RBRACE
       YKNOWHAT LPAREN expr RPAREN LBRACE stmts RBRACE
       FORGETABOUTIT LBRACE stmts RBRACE)
      (list 'IfStmt $3 $6 (list (list 'ElseIf $10 $13)) $17)))
    ;; <var_def> -> <type> <id> = <expr>
    (var_def
     ((type ID ASSIGN expr) (list 'VarDef $1 $2 $4)))
    
    ;; <type> -> cut | word | capisce | crew
    (type
     ((CUT)     'cut)
     ((WORD)    'word)
     ((CAPISCE) 'capisce)
     ((CREW)    'crew))
    
    ;; <expr> -> <expr> + <term> | <term>
    (expr
    ((expr EQ  term) (list 'BinOp '== $1 $3))
     ((expr NEQ term) (list 'BinOp '!= $1 $3))
     ((expr GT  term) (list 'BinOp '>  $1 $3))
     ((expr LT  term) (list 'BinOp '<  $1 $3))
     ((expr GTE term) (list 'BinOp '>= $1 $3))
     ((expr LTE term) (list 'BinOp '<= $1 $3))
     ((expr PLUS term)  (list 'BinOp '+ $1 $3))
     ((expr MINUS term) (list 'BinOp '- $1 $3))
     ((term)            $1))
    
    ;; <term> -> <term> * <factor> | <factor>
    (term
     ((term TIMES factor)  (list 'BinOp '* $1 $3))
     ((term DIVIDE factor) (list 'BinOp '/ $1 $3))
     ((factor)             $1))
    
    ;; <factor> -> <id> | <number> | <string> | ( <expr> )
    (factor
     ((NUMBER)             (list 'NumLiteral $1))
     ((ID)                 (list 'VarAccess $1))
     ((STRING)             (list 'StringLiteral $1))
     ((LEGIT)   (list 'BoolLiteral #t))
     ((FUGAZZI) (list 'BoolLiteral #f))
     ((LPAREN expr RPAREN) $2))
    
    )))