#lang racket/base

(require ee-lib/define (for-syntax racket/base))

(provide int bool ->)
(provide (for-syntax spl-type-literals))

;;; Due to a confluence of the following constraints (among possibly others):
;;;  [1] struct-out cannot be used inside of (provide (for-syntax))
;;;  [2] define-literal-forms generates transformer bindings for each literal
;;;      that the spl DSL needs to make availible at phase 0
;;;  [3] define-literal-forms generates a phase 1 binding for the literal set
;;;      (a syntax-parse feature) that spl-type.rkt needs at phase 0
;;;  [4] spl-type.rkt has bindings that must be at its phase 0 since it needs to
;;;      provide structs with struct-out (see contraint [1])
;;;
;;;  I found it was easier to just create a separate module for just the spl
;;;  type literals. There might be a way to set things up so that this is not
;;;  necessary, but I do not know of one. This was all caused by the want to
;;;  separate the type checking code into a separate module, which led me to
;;;  bump into the constraints described above.

(define-literal-forms spl-type-literals
  "Cannot use an spl type literal in Racket"
  (int bool ->))
