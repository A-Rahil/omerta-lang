#lang racket/base

(require "lexer.rkt")
(require "parser.rkt")
(require "evaluator.rkt") ;; <--- IMPORT THE EVALUATOR!

(define (run-omerta code-string)
  (displayln "--- COMPILING OMERTA ---")
  
  (define in-port (open-input-string code-string))
  
  ;; 1 & 2. Lex and Parse
  (define ast 
    (omerta-parser (lambda () (omerta-lexer in-port))))
  
  (displayln "\nParser Output (AST):")
  (displayln ast)
  
  (displayln "\n--- RUNNING PROGRAM ---")
  ;; 3. Evaluate (Execute the code!)
  (evaluate ast)
  
  (displayln "--- EXECUTION COMPLETE ---"))

#| ;; Test basic variables and math
(run-omerta "
the-sitdown {
  cut target = 5 * 2 + 1 !
  
  say (target) {
    holler (target) !
  } !
}
") |#