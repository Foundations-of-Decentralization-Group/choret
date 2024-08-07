#lang scribble/manual

@(require racket/require
          (for-label choret
                     (prefix-in racket/ racket)))

@title{Choret: Functional Choreographic Programming for Racket}

Choret is an in-development Racket library which allows for choreographic
programming. It implements an untyped language based off of @italic{Pirouette},
a functional choreographic programming language developed by Andrew K. Hirsch
(my research advisor) and Deepak Garg. Their paper, @italic{Pirouette:
Higher-Order Typed Functional Choreographies}, can be found here:
@url{https://dl.acm.org/doi/10.1145/3498684}.

@section{Getting Started}

todo

@section{Hello World! Example}

@racketblock[
(chor (A B)
      (define (at A msg) "Hello World!")
      (define (at B print-msg) (lambda (msg) (println msg)))

      (let ([(at B x) (~> (at A msg) B)])
        (at B (print-msg x))))
 ]

@section{Choret Forms}

@defmodule[choret]

@defform[(chor (location ...) global-body ...+)]{
 Entry point for executing Choret programs and using Choret forms. The other
 Choret forms described below only work inside the body of this macro; otherwise
 they either are undefined (raise a syntax error) or have the meaning of their
 regular Racket counterpart (e.g. @racket[define], @racket[lambda], etc.). The
 list of locations @racket[location ...] defines the set of all
 processes/locations that participate in the choreography.
}

@defform[(at location local-expr)]{
 Evaluates @racket[local-expr] at the process location @racket[location]. Not to
 be confused with the use of at forms used in binding locations such as in
 @racket[define], @racket[lambda], and @racket[let], which expect an identifier.
}

@defform[#:literals (at)
         (~> (at A local-expr) B)]{
 Evaluates @racket[local-expr] at location @racket[A], and returns the value of
 @racket[local-expr] located at location @racket[B]. Essentially, @racket[~>]
 synchronously sends a value from one location to another.
}

@defform[#:literals (at)
         (if (at location test-local-expr) then-global-expr else-global-expr)]{
 Evaluates @racket[test-local-expr] at @racket[location], which is the "guard"
 location. If a value other than @racket[#f] is produced the
 @racket[then-global-expr] is evaluated and becomes the result ofthe @racket[if]
 form. Otherwise @racket[else-global-expr] is evaluated and becomes the result
 of the @racket[if] form. Expressions involving locations other than the "guard"
 location must either be identical in each branch of the @racket[if] form, or
 must reside in a @racket[sel~>] form which mentions it as a reciever.
}

@defform[#:literals (at)
         (sel~> sender [reciever label global-expr] ...)]{
 Communicates knowledge of choice from the location @racket[sender] to each
 @racket[reciever]. @racket[label] is a symbol that represents which branch of
 execution an @racket[if] branch takes.
}

@defform*[#:literals (at)
          ((define (at location local-id) global-expr)
           (define global-id global-expr))
          #:contracts ([global-expr any]
                       [global-expr (located-at? A)])]{
 Evaluates @racket[global-expr] and either binds it to the choreographic
 variable @racket[global-id] or binds it to the local variable
 @racket[local-id] located at @racket[location].
}

@defform*[#:literals (at)
          ((set! (at location local-id) global-expr)
           (set! global-id global-expr))]{
 Sets/mutates the value of either a local variable @racket[local-id] located at
 @racket[location] or a choreographic variable @racket[global-id].
}

@defform[#:literals (at)
         (let ([at-id global-expr] ...) global-body ...+)]{
 Evaluates the @racket[global-expr]s left-to-right, where each expression
 evaluates to a located value, binding the result of each to the identifier of
 its corresponding @racket[at-id]. It is expected that the location of each
 value matches its corresponding @racket[at-id].
}

@defform[#:literals (at)
         (lambda (binding-form ...) global-body ...+)
         #:grammar ([binding-form
                     (code:line global-id)
                     (at location local-id)])]{
 Creates a choreographic function which accepts zero or more choreographic
 values. Each @racket[binding-form] is either a located @racket[local-id], which
 represents an argument which expects a value located at @racket[location], or a
 @racket[global-id], which is a global choreographic variable.
}

