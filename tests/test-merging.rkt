#lang racket/base

(require rackunit
         "../util/check-syntax.rkt"
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax))

;; Creates a single test for nested merging.  Each test case tests the merging
;; of a single "if" form. In each branch there are two "sel~>" forms, one nested
;; inside the other. The outer "sel~>" informs location B and the inner "sel~>"
;; informs location C. [VALID?] is a boolean which describes whether the
;; generated test should expect a syntax error or not. The rest of the options
;; are also booleans, each describing how to vary the syntax to test different
;; cases of nested merges.
(define-syntax (test-nested-merging stx)
  (syntax-parse stx
    [(_ VALID?
        DIFF-LABEL-B?
        DIFF-LABEL-C?
        DIFF-PROJ-B?
        DIFF-PROJ-C?)
     (let ([valid? (syntax->datum #'VALID?)]
           [dlb? (syntax->datum #'DIFF-LABEL-B?)]
           [dlc? (syntax->datum #'DIFF-LABEL-C?)]
           [dpb? (syntax->datum #'DIFF-PROJ-B?)]
           [dpc? (syntax->datum #'DIFF-PROJ-C?)])
       (with-syntax*
           ([EXPR-TRUE #'(begin (~> (at B 0) C) (~> (at C 0) B))]
            [EXPR-FALSE-B (if dpb? #'(~> (at B 1) C) #'(~> (at B 0) C))]
            [EXPR-FALSE-C (if dpc? #'(~> (at C 1) B) #'(~> (at C 0) B))]
            [EXPR-FALSE #'(begin EXPR-FALSE-B EXPR-FALSE-C)]
            [LABEL-B-FALSE (if dlb? #''b1 #''b0)]
            [LABEL-C-FALSE (if dlc? #''c1 #''c0)]
            [TEST-CODE
             #'((require choret)
                (chor (A B C)
                      (if (at A #t)
                          (sel~> A ([B 'b0])
                                 (sel~> A ([C 'c0]) EXPR-TRUE))
                          (sel~> A ([B LABEL-B-FALSE])
                                 (sel~> A ([C LABEL-C-FALSE]) EXPR-FALSE)))))]
            [TEST-CODE-ALT-SYNTAX
             #'((require choret)
                (chor (A B C)
                      (if (at A #t)
                          (sel~> A ([B 'b0] [C 'c0])
                                 EXPR-TRUE)
                          (sel~> A ([B LABEL-B-FALSE] [C LABEL-C-FALSE])
                                 EXPR-FALSE))))])
         #`(begin
             (test-case
              #,(string-append
                 "Nested Merges:\n"
                 (format  "  Diff Label B: ~a\n" dlb?)
                 (format  "  Diff Label C: ~a\n" dlc?)
                 (format  "  Diff Projection B: ~a\n" dpb?)
                 (format  "  Diff Projection C: ~a\n" dpc?))
              #,(if valid?
                    #`(check-not-syntax-error #,@#'TEST-CODE)
                    #`(check-has-syntax-error #,@#'TEST-CODE)))
             (test-case
              #,(string-append
                 "Nested Merges with Alternative Syntax:\n"
                 (format  "  Diff Label B: ~a\n" dlb?)
                 (format  "  Diff Label C: ~a\n" dlc?)
                 (format  "  Diff Projection B: ~a\n" dpb?)
                 (format  "  Diff Projection C: ~a\n" dpc?))
              #,(if valid?
                    #`(check-not-syntax-error #,@#'TEST-CODE-ALT-SYNTAX)
                    #`(check-has-syntax-error #,@#'TEST-CODE-ALT-SYNTAX))))))]))

;; Macro to specify multiple "test-nested-merging" tests at once.
(define-syntax-rule (nested-merging-tests [ARG ...] ...)
  (begin (test-nested-merging ARG ...) ...))

(nested-merging-tests
 ; Valid | Diff Label B | Diff Label C | Diff Proj B | Diff Proj C
 [  #t          #f             #f            #f            #f     ]
 [  #f          #f             #f            #f            #t     ]
 [  #f          #f             #f            #t            #f     ]
 [  #f          #f             #f            #t            #t     ]
 ; ---------------------------------------------------------------
 [  #t          #f             #t            #f            #f     ]
 [  #t          #f             #t            #f            #t     ]
 [  #f          #f             #t            #t            #f     ]
 [  #f          #f             #t            #t            #t     ]
 ; ---------------------------------------------------------------
 [  #t          #t             #f            #f            #f     ]
 [  #f          #t             #f            #f            #t     ]
 [  #t          #t             #f            #t            #f     ]
 [  #f          #t             #f            #t            #t     ]
 ; ---------------------------------------------------------------
 [  #t          #t             #t            #f            #f     ]
 [  #t          #t             #t            #t            #f     ]
 [  #t          #t             #t            #f            #t     ]
 [  #t          #t             #t            #t            #t     ])
