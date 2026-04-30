#lang racket/base

(require "lexer.rkt")
(require "parser.rkt")
(require "evaluator.rkt") 

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

;; Test

#|

; Expecting O/P: 11
(run-omerta "
the-sitdown {
  cut target = 5 * 2 + 1 !
  
  say (target) {
    holler (target) !
  } !
}
")

;Expecting O/P: 4100
(run-omerta "the-sitdown {
  //between-us Calculating the weekly take
  cut collections = 5000 !
  cut expenses = 1200 / 2 + 100 * 3 !
  cut profit = collections - expenses !
  
  holler (\"Weekly Profit:\") !
  holler (profit) !
} ")

(run-omerta" the-sitdown {
  cut sloppy_work = 50 
  holler (sloppy_work) !
} ")

; Expecting O/P:
; Wiretap located. Initiating sweep.
; Relocating the crew. Cost:
; 15000

(run-omerta" the-sitdown {
  cut wiretap_found = 1 !
  
  say (wiretap_found) {
    holler (\"Wiretap located. Initiating sweep.\") !
    cut safe_houses = 3 !
    
    //between-us A nested check
    say (safe_houses) {
       cut relocation_cost = safe_houses * 5000 !
       holler (\"Relocating the crew. Cost:\") !
       holler (relocation_cost) !
    } !
  } !
} ")

; Expecting O/P: Syntax error! Fugazzi token found: HOLLER (value: #f)
(run-omerta" the-sitdown {
  cut sloppy_work = 50 
  holler (sloppy_work) !
} ")

|#
