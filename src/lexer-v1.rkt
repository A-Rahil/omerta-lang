#lang racket/base
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

;; DEFINING THE TOKEN CATEGORIES:
;; 1. VALUE TOKENS (Carry data: variables, numbers, strings)
(define-tokens value-tokens
  (ID NUMBER STRING))

;; 2. EMPTY TOKENS (Keywords, Operators, Punctuation)
(define-empty-tokens punct-tokens
  (;; Types & Values
   CUT WORD CAPISCE CREW
   LEGIT FUGAZZI
   
   ;; Control Flow
   SAY FORGETABOUTIT YKNOWHAT
   
   ;; Functions & I/O
   JOB DELIVER CALL_ON HOLLER WHADDYA_SAY THE_SITDOWN
   
   ;; Operators
   PLUS MINUS TIMES DIVIDE
   BUMP SHAVE ASSIGN
   
   ;; Punctuation
   LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA BANG EOF))

;; 3. CHARACTER CLASSES (Regular Expressions)
(define-lex-abbrevs
  (digit      (:/ "0" "9"))
  (letter     (:or (:/ "a" "z") (:/ "A" "Z")))
  (id-char    (:or letter digit "_")))

;; 4. THE LEXER
(define omerta-lexer
  (lexer
   ;; Whitespace: Skip spaces, tabs, and newlines
   [(:+ (:or " " "\t" "\n" "\r"))
    (omerta-lexer input-port)]

   ;; Comments: //between-us ignores everything until a newline
   [(:seq "//between-us" (:* (:~ "\n")))
    (omerta-lexer input-port)]

   ;; --- KEYWORDS ---
   ["cut"           (token-CUT)]
   ["word"          (token-WORD)]
   ["capisce"       (token-CAPISCE)]
   ["crew"          (token-CREW)]
   ["legit"         (token-LEGIT)]
   ["fugazzi"       (token-FUGAZZI)]
   ["say"           (token-SAY)]
   ["forgetaboutit" (token-FORGETABOUTIT)]
   ["yknowhat"      (token-YKNOWHAT)]
   ["job"           (token-JOB)]
   ["deliver"       (token-DELIVER)]
   ["call-on"       (token-CALL_ON)]
   ["holler"        (token-HOLLER)]
   ["whaddya-say"   (token-WHADDYA_SAY)]
   ["the-sitdown"   (token-THE_SITDOWN)]
   ["bump"          (token-BUMP)]
   ["shave"         (token-SHAVE)]

   ;; --- OPERATORS & PUNCTUATION ---
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
   ["!" (token-BANG)] ;; Your statement terminator!

   ;; --- DYNAMIC TOKENS (Variables, Numbers, Strings) ---
   ;; Number: 1 or more digits
   [(:+ digit) 
    (token-NUMBER (string->number lexeme))]
   
   ;; Identifier: Starts with a letter, followed by letters/digits/underscores
   [(:seq letter (:* id-char)) 
    (token-ID lexeme)]
   
   ;; String: A quote, any characters that aren't a quote, and a closing quote
   [(:seq "\"" (:* (:~ "\"")) "\"")
    (token-STRING (substring lexeme 1 (- (string-length lexeme) 1)))]

   ;; End of File
   [(eof) (token-EOF)]))
