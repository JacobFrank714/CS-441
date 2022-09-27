#lang racket
;;; IMPORT
;; Import the lexer tools 
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)  ; names from lex-sre are prefixed with :
         ;                                     to avoid name collisions
         )

;;; REGULAR EXPRESSIONS

;; Names for regular expressions matching letters and digits.
;; Note that :or are prefixed with a : due to (prefix-in : ...) above
(define-lex-abbrevs
  [read       "read"]
  [write      "write"]
  [letter     (:or (:/ "a" "z") (:/ #\A #\Z) "?" "!")]
  [digit      (:/ #\0 #\9)]
  [mult-op    (or "*" "/")]
  [add-op     (or "+" "-")]
  [end-of-file "$$"]
  )

;;; TOKENS

;; Tokens such as numbers (and identifiers and strings) carry a value
;; In the example only the NUMBER token is used, but you may need more.
(define-tokens value-tokens (NUMBER END-OF-PROGRAM READ WRITE IDENTIFIER MULT-OP ADD-OP))

;; Tokens that don't carry a value.
(define-empty-tokens op-tokens (newline :=  = < > + - * / ^  EOF))

;;; LEXER

;; Here the lexer (aka the scanner) is defined.
;; The construct lexer-src-pos evaluates to a function which scans an input port
;; returning one position-token at a time.

;; A position token contains besides the actual token also source location information
;; (i.e. you can see where in the file the token was read)

(define lex
  (lexer-src-pos
    [(eof)                                            ; input: eof of file     
    'EOF]                                            ; output: the symbol EOF

    [(:+ end-of-file)
    (token-END-OF-PROGRAM (string->symbol lexeme))]

    [(:or #\tab #\space #\newline "\r")               ; input: whitespace
    (return-without-pos (lex input-port))]           ; output: the next token
   ;                                                           (i.e. skip the whitespace)

    [#\newline                                        ; input: newline
    (token-newline)]                                 ; ouput: a newline-token   
   ;                                                 ; note:  (token-newline) returns 'newline

    [(:or ":=" "*" "/" "^" "<" ">" "=")       ; input:  an operator
    (string->symbol lexeme)]                         ; output: corresponding symbol

    [(:or "+" "-")                                   ; input: "+" or "-"
    (token-ADD-OP (string->symbol lexeme))]         ; ouput: an ADD-OP token

    [(:or "*" "/")                                  ; input: "*" or "/"
    (token-MULT-OP (string->symbol lexeme))]        ; output: a MULT-OP token

    [(:+ digit)                                     ; input:  digits
    (token-NUMBER (string->number lexeme))]         ; outout: a NUMBER token whose value is the number

    [(:+ read)                                      ; input: the string "read"
    (token-READ lexeme)]                            ; output: READ token

    [(:+ write)                                     ; input: the string "write"
    (token-WRITE lexeme)]                           ; output: WRITE token

    [(:+ letter)                                    ; input: Alphabetic letter
    (token-IDENTIFIER lexeme)]                      ; output: IDENTIFIER token whose value is the word

  ))

;;; TEST

(define input (open-input-file "test.txt"))

(lex input) ; (position-token (token 'NUMBER 123) (position 1 #f #f) (position 4 #f #f))
(lex input) ; (position-token '+ (position 4 #f #f) (position 5 #f #f))
(lex input) ; (position-token (token 'NUMBER 456) (position 5 #f #f) (position 8 #f #f))
(lex input) ; (position-token 'EOF (position 8 #f #f) (position 8 #f #f))


;; Let's make it a little easier to play with the lexer.

(define (string->tokens s)
  (port->tokens (open-input-file s)))

(define (port->tokens in)
  (define token (lex in))
  (if (eq? (position-token-token token) 'EOF)
      '()
      (cons token (port->tokens in))))

(provide string->tokens)
(map position-token-token (string->tokens "test.txt"))   ; strip positions
