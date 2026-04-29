#lang racket/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

;; ==========================================
;; TOKEN CATEGORIES
;; ==========================================

;; 1. VALUE TOKENS — carry data (identifier name, number value, string contents)
(define-tokens value-tokens
  (ID NUMBER STRING))

;; 2. EMPTY TOKENS — keywords, operators, punctuation (no data payload)
(define-empty-tokens punct-tokens
  (;; -- Types --
   CUT       ;; integer  (cut x = 5)
   WORD      ;; string   (word name = "Tony")
   CAPISCE   ;; boolean  (capisce flag = legit)
   CREW      ;; list     (crew myList = ...)

   ;; -- Boolean literals --
   LEGIT     ;; true
   FUGAZZI   ;; false

   ;; -- Control Flow --
   SAY           ;; if
   FORGETABOUTIT ;; else
   YKNOWHAT      ;; while

   ;; -- Functions & I/O --
   JOB         ;; function definition
   DELIVER     ;; return
   CALL_ON     ;; function call
   HOLLER      ;; print
   WHADDYA_SAY ;; input
   THE_SITDOWN ;; program entry point

   ;; -- Arithmetic Operators --
   PLUS MINUS TIMES DIVIDE

   ;; -- Compound Assignment --
   BUMP   ;; +=
   SHAVE  ;; -=

   ;; -- Assignment --
   ASSIGN  ;; =

   ;; -- Punctuation --
   LPAREN RPAREN   ;; ( )
   LBRACE RBRACE   ;; { }
   LBRACK RBRACK   ;; [ ]
   COMMA           ;; ,
   BANG            ;; ! — statement terminator

   EOF))

;; ==========================================
;; CHARACTER CLASS ABBREVIATIONS
;; ==========================================

(define-lex-abbrevs
  (digit   (:/ "0" "9"))
  (letter  (:or (:/ "a" "z") (:/ "A" "Z")))
  (id-char (:or letter digit "_")))

;; ==========================================
;; THE LEXER
;; ==========================================
;; Rules are checked TOP-TO-BOTTOM; first match wins.
;; Keywords must appear BEFORE the identifier rule so that
;; "cut" is never tokenised as an ID.

(define omerta-lexer
  (lexer

   ;; -- Whitespace: skip spaces, tabs, newlines --
   [(:+ (:or " " "\t" "\n" "\r"))
    (omerta-lexer input-port)]

   ;; -- Comments: //between-us ... (to end of line) --
   [(:seq "//between-us" (:* (:~ "\n")))
    (omerta-lexer input-port)]

   ;; -- Multi-word keywords (must come before single words & operators) --
   ["the-sitdown"   (token-THE_SITDOWN)]
   ["call-on"       (token-CALL_ON)]
   ["whaddya-say"   (token-WHADDYA_SAY)]
   ["forgetaboutit" (token-FORGETABOUTIT)]

   ;; -- Single-word keywords --
   ["cut"      (token-CUT)]
   ["word"     (token-WORD)]
   ["capisce"  (token-CAPISCE)]
   ["crew"     (token-CREW)]
   ["legit"    (token-LEGIT)]
   ["fugazzi"  (token-FUGAZZI)]
   ["say"      (token-SAY)]
   ["yknowhat" (token-YKNOWHAT)]
   ["job"      (token-JOB)]
   ["deliver"  (token-DELIVER)]
   ["holler"   (token-HOLLER)]
   ["bump"     (token-BUMP)]
   ["shave"    (token-SHAVE)]

   ;; -- Operators & Punctuation --
   ["+" (token-PLUS)]
   ["-" (token-MINUS)]
   ["*" (token-TIMES)]
   ["/" (token-DIVIDE)]
   ["=" (token-ASSIGN)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   ["{" (token-LBRACE)]
   ["}" (token-RBRACE)]
   ["[" (token-LBRACK)]
   ["]" (token-RBRACK)]
   ["," (token-COMMA)]
   ["!" (token-BANG)]

   ;; -- Numbers: one or more digits --
   [(:+ digit)
    (token-NUMBER (string->number lexeme))]

   ;; -- Identifiers: letter followed by letters/digits/underscores --
   ;; Must come AFTER all keyword rules.
   [(:seq letter (:* id-char))
    (token-ID lexeme)]

   ;; -- String literals: "..." (no escaped quotes supported yet) --
   [(:seq "\"" (:* (:~ "\"")) "\"")
    (token-STRING (substring lexeme 1 (- (string-length lexeme) 1)))]

   ;; -- End of file --
   [(eof) (token-EOF)]))

;; ==========================================
;; EXPORTS
;; ==========================================

(provide omerta-lexer value-tokens punct-tokens)
