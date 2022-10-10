#lang racket

(require "lexer.rkt")
(require parser-tools/lex)

(define (setter? x)
    (print "checking setter? ")
    (println (first x))
    (if (eq? (first x) ':=)
        (expr? (rest x))
        #f
    )
)

(define (id? x)
    (print "checking id? ")
    (println (first x))
    (if (eq? (token-name (first x)) 'IDENTIFIER)
        (rest x)
        #f
    )
)

(define (num? x)
    (print "checking num? ")
    (println (first x))
    (if (eq? (token-name (first x)) 'NUMBER)
        (rest x)
        #f
    )
)

(define (paren-end? x)
    (print "checking paren-end? ")
    (println (first x))
    (if (eq? (token-name (first x)) 'PAREN-END)
        (rest x)
        #f
    )
)

(define (paren-start? x)
    (print "checking paren-start? ")
    (println (first x))
    (if (eq? (token-name (first x)) 'PAREN-START)
        (expr? (rest x))
        #f
    )
)

(define (mult_op? x)
    (print "checking mult_op? ")
    (println (first x))
    (cond
        [(eq? (token-name (first x)) 'MULT-OP) (factor? (rest x))]
        [else #f]
    )
)

(define (add_op? x)
    (print "checking add_op? ")
    (println (first x))
    (cond   
        [(eq? (token-name (first x)) 'ADD-OP) (term? (rest x))]
        [else #f]
    )
)

(define (factor_tail? x)
    (print "checking factor_tail? ")
    (println (first x))
    (cond
        [(paren-end? x) (rest x)]
        [(mult_op? x) (factor? (rest x))]
        [(eq? (token-name (first x)) 'newline) (rest x)]
        [else (rest x)]
    )
)

(define (factor? x)
    (print "checking factor? ")
    (println (first x))
    (cond
        ;;; [(eq? (token-name (first x)) 'PAREN-START) (expr? (rest x))]
        [(id? x) (rest x)]
        [(num? x) (rest x)]
        [else #f]
    )
)

(define (term_tail? x)
    (print "checking term_tail? ")
    (println (first x))
    (cond
        [(add_op? x) (term? (rest x))]
        [(paren-end? x) (rest x)]
        [(eq? (token-name (first x)) 'newline) (rest x)]
        [else (rest x)]
    )
)

(define (term? x)
    (print "checking term? ")
    (println (first x))
    (cond 
        [(paren-start? x) (expr? (rest x))]
        [(factor? x) (if (eq? (token-name (second x)) 'ADD-OP) 
                        (add_op? (rest x)) 
                        (factor_tail? (rest x)))]
        [else #f]
    )
)

(define (expr? x)
    (print "checking expr? ")
    (println (first x))
    (cond
        [(term? x) (term_tail? (rest x))]
        [else #f]))



(define (stmt? x)
    (print "checking stmt? ")
    (println (first x))
    (cond 
        [(eq? (token-name (first x)) 'IDENTIFIER) (setter? (rest x))]
        [(eq? (token-name (first x)) 'READ) (id? (rest x))]
        [(eq? (token-name (first x)) 'WRITE) (expr? (rest x))]
        [else #f]
    )
)

(define (stmt_list? x)
    (print "checking stmt_list? ")
    (println (first x))
    (cond
        [(stmt? x) (stmt_list? (rest x))]
        [(eq? (token-name (first x)) 'newline) (rest x)]
        [else #f]
    )
)

(define (program? x) 
    (print "checking program? ")
    (println (first x))
    (if (stmt_list? x)
        (if(eq? (token-name last x) 'END-OF-PROGRAM) 
            #t 
            #f
        ) 
        #f
    )
)

(define (parse file)
    (define tokens(map position-token-token (string->tokens file))) 
    ;;; tokens
    (define prog? (program? tokens))
    prog?
)
