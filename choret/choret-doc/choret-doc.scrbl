#lang scribble/manual

@(require racket/require
          (for-label choret
                     (prefix-in racket/ racket)))

@title{Choret: Functional Choreographic Programming for Racket}

Choret is an in-development Racket library which allows for choreographic programming. It implements an untyped language based off of @italic{Pirouette}, a functional choreographic programming language developed by Andrew K. Hirsch (my research advisor) and Deepak Garg. Their paper, @italic{Pirouette: Higher-Order Typed Functional Choreographies}, can be found here: @url{https://dl.acm.org/doi/10.1145/3498684}.

@section{Getting Started}

@subsection{Requirements to Run}

Currently Choret's unit tests have been tested and passed on Racket versions 6.6, 6.12, 7.0, 7.4, 7.9, 8.0, 8.2, and 8.14. Choret may work as intended on other versions of Racket as well, although, if you are experiencing issues on a different version of Racket, then you might want to consider using the the @tt{cross} package: @url{https://docs.racket-lang.org/raco-cross/index.html} with the @verbatim|{--version}| flag or consider installing a newer version of Racket. NOTE: Choret is not guaranteed to remain compatible with the versions of Racket listed above as this library is experimental.

@subsection{Installing Choret as a Library}

As of right now Choret is not yet availible via the Racket Package Index. However, Choret can be installed by using the command:
@commandline{raco pkg install https://github.com/Foundations-of-Decentralization-Group/choret.git?path=choret}
which instructs @tt{raco} to install it directly from the GitHub repository.

Alternatively, Choret can be installed manually. First clone the Choret repository: @url{https://github.com/Foundations-of-Decentralization-Group/choret}. Once the repository has been cloned, say to a directory named "choret", there should be another sub-directory named "choret"; change directory (@tt{cd}) into that directory and run the following command:
@commandline{raco pkg install}
which will install the Choret package as a link to that directory.

For more information about installing packages using @tt{raco}, see the following resources from the Racket documentation:
@itemlist[
  @item{Using @tt{raco pkg install}: @url{https://docs.racket-lang.org/pkg/cmdline.html}}
  @item{Package Sources: @url{https://docs.racket-lang.org/pkg/Package_Concepts.html#(part._concept~3asource)}}
]

@section{What is Choreographic Programming?}

In a traditional system with multiple @italic{locations} (threads, processes, etc.) interacting with each other, the programmer has to carefully write multiple independent programs while also avoiding things such as deadlock or livelock. In particular, encoding a pattern of sends and recieves between locations in a system can be error prone since it is relatively easy to write mismatched send and recieve actions between two separately maintained programs; it is entirely up to the programmer to maintain the state and invariants of the entire system.

Choreographic programming aims to solve this problem by allowing programmers to give a "global" description of the program. In other words, the programmer writes a single program that describes the logic of the system/protocol as a whole, explicity specifying the when data needs to be transferred from one participant to another.

@subsection{The Bookseller Example in Racket}

Consider the quintessential example of an online bookseller. In order to sell a
book, the following should happen between a buyer and the seller:
@itemlist[
  @item{The buyer sends the title of a book they would like to buy to the
  seller.}
  @item{The seller looks up the book title and retrieves the cost of the book;
  the price is communicated back to the buyer.}
  @item{The buyer tells the seller whether or not they will buy the book.}
  @item{If the buyer says they will buy the book, the seller completes the
  transaction by giving the buyer a shipping date.}
 #:style 'ordered
]

Seems simple enough, this is what such a program might look like in Racket, with the matching sends and recieves commented with a number in square brackets (using threads and channels for simplicity):

@#reader scribble/comment-reader
(racketblock
(define ch (make-channel))

;; Buyer thread
(define buyer
  (thread
   (lambda ()
     (define book-title "Alice in Wonderland")
     (define budget 20)

     (channel-put ch book-title) ;; [1]
     (define price (channel-get ch)) ;; [2]
     (if (< price budget)
         (let ()
           (channel-put ch 'buy-book) ;; [3]
           (define date (channel-get ch))
           (printf "My book should arrive on ~a!~n" date)) ;; [4]
         (printf "Nevermind!~n")))))

;; Seller thread
(define seller
  (thread
   (lambda ()
     (define price-catalog (make-hash '(["Alice in Wonderland" . 25])))

     (define book-title (channel-get ch)) ;; [1]
     (define price (hash-ref price-catalog book-title))
     (channel-put ch price) ;; [2]
     (define will-buy (channel-get ch)) ;; [3]
     (if (equal? will-buy 'buy-book)
         (channel-put ch "January 1, 1970") ;; [4]
         (printf "Let me know if you change your mind!~n")))))

(thread-wait buyer)
(thread-wait seller)
)

This seems correct, and given that the buyer's budget (20) is lower than the cost of the book (25) the expected result of this program would be:
@commandline{Nevermind!}
@commandline{Let me know if you change your mind!}
However the actual output of
this program is:
@commandline{Nevermind!}
What's the problem? Well, the buyer needs to let the seller know if they are going to buy the book, but in the buyer thread code, the buyer only communicates the @italic{knowledge of this choice} if decides to buy the book; when the buyer does not buy the book it leaves the seller deadlocked waiting on the buyer's decision.

Even though this may be a trivial example, it emphasizes the importance of correctly matching sends and recieves; in much more complicated programs it may be much harder to correctly do this by hand.

@subsection{The Bookseller Example in Choret}

If manually matching sends and recieves is error prone, what can be done differently?

Looking back in the last section, the bookseller system was described as a series of steps:

@itemlist[
  @item{The buyer sends the title of a book they would like to buy to the
  seller.}
  @item{The seller looks up the book title and retrieves the cost of the book;
  the price is communicated back to the buyer.}
  @item{The buyer tells the seller whether or not they will buy the book.}
  @item{If the buyer says they will buy the book, the seller completes the
  transaction by giving the buyer a shipping date.}
 #:style 'ordered
]

This English prose description already describes what the system should do in a global manner. This can be translated into pseudo-code:

@verbatim|{
buyer.title ~> seller.title
seller.catalog(title) ~> buyer.price
if buyer.price < buyer.budget then
    buyer informs seller "I'll buy the book"
    seller.getDate(title) ~> buyer.date
    buyer.print(date)
else
    buyer informs seller "I won't buy the book"
    seller.print("Let me know if you change your mind!")
endif
}|

The above pseudo-code basically describes a choreographic program! Finally here is the bookseller program written in Choret:

@#reader scribble/comment-reader
(racketblock
(require choret)

(chor (buyer seller)
      (define (at buyer book-title) (at buyer "Alice in Wonderland"))
      (define (at buyer budget) (at buyer 30))

      (define (at seller price-catalog)
        (at seller (make-hash '(["Alice in Wonderland" . 25]))))

      (define (at seller book-title) (~> (at buyer book-title) seller)) ;; [1]
      (define (at seller price) (at seller (hash-ref price-catalog book-title)))
      (define (at buyer price) (~> (at seller price) buyer))
      (if (at buyer (< price budget))
          (sel~> buyer
                 [seller 'buy-book
                         (let ()
                           (define (at seller date)
                             (at seller "January 1, 1970"))
                           (define (at buyer date)
                             (~> (at seller date) buyer))
                           (at buyer
                               (printf
                                "My book should arrive on ~a!~n"
                                date)))])
          (sel~> buyer
                 [seller 'reject-book
                         (begin
                           (at buyer (printf "Nevermind!~n"))
                           (at seller
                               (printf
                                "Let me know if you change your mind!~n")))])))
)

This is a somewhat verbose piece of code so here is a breakdown of what is going on:

First, there are these @racket[(define (at LOCATION ID) EXPR)] forms which define a variable which is located at @racket[LOCATION]; @racket[EXPR] can be any Choret form which evaluates to a value that is also located at @racket[LOCATION].

There are also communication primitives of the form @racket[(~> (at SENDER LOCAL-EXPR) RECIEVER)], these forms evaluate @racket[LOCAL-EXPR] at the location @racket[SENDER] and produce a value located at @racket[RECIEVER]; the underlying implementation executes a @racket[~>] form by sending the result of evaluating @racket[LOCAL-EXPR] and sending the value from @racket[SENDER] to @racket[RECIEVER].

"at" expressions, which have the form @racket[(at LOCATION LOCAL-EXPR)] evaluate @racket[LOCAL-EXPR] as a normal Racket expression located at @racket[LOCATION].

The forms that comprise the first half of the choreography are fairly straightforward, however, things get a little more involved when handling conditionals.

@subsection{Conditional Expressions}

What makes conditional branching difficult in choreographic programming? It helps to think about the code that has to be generated for each location, which is referred to as the @italic{projection} of each location.

Take for example the form
@racketblock[
(if (at L1 (equal? x y))
  (~> (at L1 5) L2)
  (~> (at L1 10) L2))
]

The projection for location @racket[L1] is straightforward:
@racketblock[
(if (equal? x y)
  (send L2 5)
  (send L2 10))
]

However the projection for @racket[L2] is trickier. In this case one may notice that @racket[L2]'s projection for each branch is the same:
@racketblock[
(if ???
  (recieve L1)
  (recieve L1))
]

These subforms for each branch are the same, and thus can be combinded into a single form:
@racketblock[
(recieve L1)
]

This combining of the branches of the @racket[if] form is known as @italic{merging}. In fact whenever an @racket[if] expresssion is projected for locations other than the one in the guard expression (in this case @racket[L1]), the two branches are merged together. Merging is strictly something that is performed at compile time.

That is all well and good, but what happens if one tries to project the following instead:
@racketblock[
(if (at L1 (equal? x y))
  (~> (at L2 5) L1)
  (~> (at L2 10) L1))
]
for @racket[L2]:
@racketblock[
(if ???
  (send L1 5)
  (send L1 10))
]

There are two problems here. First, since @racket[L2] now needs to do something different in each branch, there needs to be some way to communicate the knowledge of @racket[L1]'s decision to @racket[L2]. Second, @racket[L2]'s projections of the branches cannot be merged together as they are not identical (the @racket[5] and @racket[10] do not match). In the next section, @italic{selections} will be used to solve both of these problems.

@subsection{Selection and Knowledge of Choice}

The dependency between locations about which branch should be taken is known as @italic{Knowledge of Choice}. To comminicate Knowledge of Choice a message from the deciding location (such as @racket[L1] in the example above) to other dependent locations needs to be sent.

In the previous example, it is trivial to understand that @racket[L1] should send @racket[L2] a message about which branch to take. So why not have the compiler just automatically do this for the programmer? A problem arises in programs with more locations; if the compiler does this naively, then it has to send a message to every location, even if not all locations need to know the decision at @racket[L1]; otherwise the compiler needs to automatically infer which locations need Knowledge of Choice, which can be difficult to do.

Instead of doing either of those things, Choret requires the programmer to explicitly state Knowledge of Choice where appropriate. This is accomplished using the @racket[(sel~> SENDER [RECIEVER LABEL EXPR] ...)] form to perform what is known as a @italic{selection}. The selection form can be thought of as sending a message from @racket[SENDER], which should be the location which actually determines whether to take the branch, to a multiple recieving locations, where for each @racket[RECIEVER], @racket[LABEL] is a unique value that represents the branch to be taken and @racket[EXPR] is a Choret expression to be evaluated for that particular branch.

The example from the last section can be updated to use selections as appropriate:

@racketblock[
(if (at L1 (equal? x y))
  (sel~> L1 [L2 'equal (~> (at L2 5) L1)])
  (sel~> L1 [L2 'not-equal (~> (at L2 10) L1)]))
]
which when projected for @racket[L2] would be something like:
@racketblock[
(merge
  (sel~> L1 [L2 'equal (~> (at L2 5) L1)])
  (sel~> L1 [L2 'not-equal (~> (at L2 10) L1)]))
]
which would be merged into:
@racketblock[
(sel~> L1 [L2 'equal (~> (at L2 5) L1)]
          [L2 'not-equal (~> (at L2 10) L1)])
]
which would expand into:
@racketblock[
(let ([L1-decision (recieve L1)])
  (cond [(equal? L1-decision 'equal)
         (~> (at L2 5) L1)]
        [(equal? L1-decision 'not-equal)
         (~> (at L2 10) L1)]))
]

As seen above, selections are also important for merging. In fact the case where two selections (@racket[sel~>] forms) are being merged together is actually a special case; the set labels between the two selections are compared and the sub-expressions corresponding to unique labels are simply left alone to be projected as-is; if two labels are shared between the selections however, the sub-expressions corresponding to those labels are recursively merged together.

In the example above, since the selections in either branch do not match, they do not need to be merged together.

It should be noted that the reason why unique labels between selections do not need to be merged is because the @racket[sel~>] form also generates the code needed to communicate the appropriate @italic{Knowledge of Choice}; merging is only needed in the absence of @italic{Knowledge of Choice}.

@subsection{Simplified Bookseller Example in Choret}

The previous bookseller example in Choret was a bit verbose and repetitive, but this can be improved by using more of the forms and idioms availible in Choret:

@#reader scribble/comment-reader
(racketblock
(chor (buyer seller)
      (define (at buyer book-title) (at buyer "Alice in Wonderland"))
      (define (at buyer budget) (at buyer 30))

      (define (at seller price-catalog)
        (at seller (make-hash '(["Alice in Wonderland" . 25]))))

      (define (at buyer price)
        (let ([(at seller book-title) (~> (at buyer book-title) seller)])
          (~> (at seller (hash-ref price-catalog book-title)) buyer)))

      (if (at buyer (< price budget))
          (sel~> buyer
                 [seller 'buy-book
                         (let ([(at buyer date)
                                (~> (at seller "January 1, 1970") buyer)])
                           (at buyer
                               (printf
                                "My book should arrive on ~a!~n"
                                date)))])
          (sel~> buyer
                 [seller 'reject-book
                         (begin
                           (at buyer (printf "Nevermind!~n"))
                           (at seller
                               (printf
                                "Let me know if you change your mind!~n")))])))
)

@section{Choret Forms}

@defmodule[choret]

@defform[(chor (location ...) global-body ...+)]{
 Entry point for executing Choret programs and using Choret forms. The other
 Choret forms described below only work inside the body of this macro; otherwise
 they either are undefined (raise a syntax error) or have the meaning of their
 regular Racket counterpart (e.g. @racket[define], @racket[lambda], etc.). The
 list of locations @racket[location ...] defines the set of all locations that
 participate in the choreography.
}

@defform[(at location local-expr)]{
 Evaluates @racket[local-expr] at the location @racket[location]. Not to be
 confused with the use of at forms used in binding locations such as in
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
         (let ([id global-expr] ...) global-body ...+)
         #:grammar ([id
                     (at location local-id)
                     global-id])]{
 Evaluates the @racket[global-expr]s left-to-right, where each expression
 evaluates to a located value, binding the result of each to the identifier of
 its corresponding @racket[id]. @racket[id] may either be a local variable or a
 choreographic variable.
}

@defform[#:literals (at)
         (lambda (binding-form ...) global-body ...+)
         #:grammar ([binding-form
                     (code:line global-id)
                     (at location local-id)])]{
 Creates a choreographic function which accepts zero or more choreographic
 values. Each @racket[binding-form] is either a located @racket[local-id], which
 represents an argument which expects a value located at @racket[location], or a
 @racket[global-id], which is a choreographic variable.
}

@section{Future Work}

Being an experimental library, there are still many things that could be added and/or ammended to improve Choret:

@itemlist[
 @item{A type system for tracking and checking located-types and choreographic function types}
 @item{Implement Location Polymorphism}
 @item{Prohibit choreographic variables from being used directly in local expressions}
 @item{More unit tests and measurement of code coverage}
 @item{Better logging functionality}
 @item{Better error messages when projecting code}
 @item{Wrap all choreographic values with some kind of "chor-value" struct to provide better runtime errors (and perhaps other functionality)}
 @item{Provide Choret as a proper "#lang" rather than just a library}
 @item{Allow Choret to create module level definitions}
 @item{Lift more forms from Racket into Choret}
 @item{Integrate Choret with Racket Places}
 @item{Implement a library of convenience functions and macros for syntactic sugar for Choret}
 @item{Improve the underlying system of macros in "threads-network.rkt" which helps with implementing locations as Racket threads with synchronous channels}
]
