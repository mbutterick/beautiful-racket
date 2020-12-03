#lang scribble/manual
@(require (for-label racket/base racket/gui/base racket/contract br br/indent))

@(require scribble/eval)

@(define my-eval (make-base-eval))
@(my-eval `(require br racket/stxparam (for-syntax br br/define)))


@title[#:style 'toc]{Beautiful Racket}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]



@link["http://beautifulracket.com"]{@italic{Beautiful Racket}} is a book about making programming languages with Racket.

This library provides the @tt{#lang br} teaching language used in the book, as well as supporting modules that can be used in other programs.

This library is designed to smooth over some of the small idiosyncrasies and inconsistencies in Racket, so that those new to Racket are more likely to say ``ah, that makes sense'' rather than ``huh? what?''

@section{Installation}

If you want all the code & documentation, install the package @racket[beautiful-racket].

If you just want the code modules (for instance, for use as a dependency in another project) install the package @racket[beautiful-racket-lib].

If you just want the @racketmodname[br/macro] and @racketmodname[br/syntax] modules, install the package @racket[beautiful-racket-macro].

@section{Conditionals}

@defmodule[br/cond]

@defform[(while cond body ...)]{
Loop over @racket[body] as long as @racket[cond] is not @racket[#f]. If @racket[cond] starts out @racket[#f], @racket[body] is never evaluated.

@examples[#:eval my-eval
(let ([x 42])
  (while (positive? x)
         (set! x (- x 1)))
  x)
(let ([x 42])
  (while (negative? x)
         (unleash-zombie-army))
  x)
]
}

@defform[(until cond body ...)]{
Loop over @racket[body] until @racket[cond] is not @racket[#f]. If @racket[cond] starts out not @racket[#f], @racket[body] is never evaluated.

@examples[#:eval my-eval
(let ([x 42])
  (until (zero? x)
         (set! x (- x 1)))
  x)
(let ([x 42])
  (until (= 42 x)
         (destroy-galaxy))
  x)
]
}

@section{Datums}

@defmodule[br/datum]

A @defterm{datum} is a literal representation of a single unit of Racket code, also known as an @defterm{S-expression}. Unlike a string, a datum preserves the internal structure of the S-expression. Meaning, if the S-expression is a single value, or list-shaped, or tree-shaped, so is its corresponding datum. 

Datums are made with @racket[quote] or its equivalent notation, the @litchar{'} prefix (see @secref["quote" #:doc '(lib "scribblings/guide/guide.scrbl")]).

When I use ``datum'' in its specific Racket sense, I use ``datums'' as its plural rather than ``data'' because that term has an existing, more generic meaning. 

@defproc[
(format-datum
[datum-form datum?]
[val any/c?] ...)
(or/c datum? void?)]{
Similar to @racket[format], but the template @racket[datum-form] is a datum, rather than a string, and the function returns a datum, rather than a string. Otherwise, the same formatting escapes can be used in the template (see @racket[fprintf]).

Two special cases. First, a string that describes a list of datums is parenthesized so the result is a single datum. Second, an empty string returns @racket[void] (not @racket[#f], because that's a legitimate datum).

@examples[#:eval my-eval
(format-datum '42)
(format-datum '~a "foo")
(format-datum '(~a ~a) "foo" 42)
(format-datum '~a "foo bar zam")
(void? (format-datum '~a ""))
(format-datum '~a #f)
]
}

@defproc[
(format-datums
[datum-form (or/c list? symbol?)]
[vals (listof any/c?)] ...)
(listof (or/c list? symbol?))]{
Like @racket[format-datum], but applies @racket[datum-form] to the lists of @racket[vals] in similar way to @racket[map], where values for the format string are taken from the lists of @racket[vals] in parallel. This means that a) @racket[datum-form] must accept as many arguments as there are lists of @racket[vals], and b) the lists of @racket[vals] must all have the same number of items.

@examples[#:eval my-eval
(format-datums '~a '("foo" "bar" "zam"))
(format-datums '(~a 42) '("foo" "bar" "zam"))
(format-datums '(~a ~a) '("foo" "bar" "zam") '(42 43 44))
(format-datums '42 '("foo" "bar" "zam"))
(format-datums '(~a ~a) '("foo" "bar" "zam") '(42))
]
}

@defproc[(datum? [x any/c]) boolean?]{
Return @racket[#t] if @racket[x] is a @racket[list?] or a @racket[symbol?].

 @examples[#:eval my-eval
(datum? '(a b c d e f))
(datum? 'a)
(datum? "a")
]
}

@section{Debugging}

@defmodule[br/debug]


@defform*[[
(report expr) 
(report expr maybe-name)
]]{
Print the name and value of @racket[expr] to @racket[current-error-port], but also return the evaluated result of @racket[expr] as usual. This lets you see the value of an expression or variable at runtime without disrupting any of the surrounding code. Optionally, you can use @racket[maybe-name] to change the name shown in @racket[current-error-port].

For instance, suppose you wanted to see how @racket[first-condition?] was being evaluted in this expression:

@racketblock[
(if (and (first-condition? x) (second-condition? x))
  (one-thing)
  (other-thing))]

You can wrap it in @racket[report] and find out:

@racketblock[
(if (and (report (first-condition? x)) (second-condition? x))
  (one-thing)
  (other-thing))]

This code will run the same way as before. But when it reaches @racket[first-condition?], you willl see in @racket[current-error-port]:

@racketerror{(first-condition? x) = #t}

You can also add standalone calls to @racket[report] as a debugging aid at points where the return value will be irrelevant, for instance:

@racketblock[
(report x x-before-function)
(if (and (report (first-condition? x)) (second-condition? x))
  (one-thing)
  (other-thing))]

@racketerror{x-before-function = 42
@(linebreak)(first-condition? x) = #t}

But be careful — in the example below, the result of the @racket[if] expression will be skipped in favor of the last expression, which will be the value of @racket[x]:

@racketblock[
(if (and (report (first-condition? x)) (second-condition? x))
  (one-thing)
  (other-thing))
(report x)]


@defform[(report* expr ...)]
Apply @racket[report] separately to each @racket[expr] in the list.


@defform*[((report-datum stx-expr) (report-datum stx-expr maybe-name))]
A variant of @racket[report] for use with @secref["stx-obj" #:doc '(lib "scribblings/guide/guide.scrbl")]. Rather than print the whole object (as @racket[report] would), @racket[report-datum] prints only the datum inside the syntax object, but the return value is the whole syntax object.
}

@section{Define}

@defmodule[br/define]

@margin-note{This module also exports the bindings from @racketmodname[br/macro].}

@defform[
(define-cases id 
  [pat body ...+] ...+)
]
Define a function that behaves differently depending on how many arguments are supplied (also known as @seclink["Evaluation_Order_and_Arity" #:doc '(lib "scribblings/guide/guide.scrbl")]{@italic{arity}}). Like @racket[cond], you can have any number of branches. Each branch starts with a @racket[_pat] that accepts a certain number of arguments. If the current invocation of the function matches the number of arguments in @racket[_pat], then the @racket[_body] on the right-hand side is evaluated. If there is no matching case, an arity error arises. (Derived from @racket[case-lambda], whose notation you might prefer.)

@examples[#:eval my-eval
(define-cases f
  [(f arg1) (* arg1 arg1)]
  [(f arg1 arg2) (* arg1 arg2)]
  [(f arg1 arg2 arg3 arg4) (* arg1 arg2 arg3 arg4)])

(f 4)
(f 6 7)
(f 1 2 3 4)
(f "three" "arguments" "will-trigger-an-error")

(define-cases f2
  [(f2) "got zero args"]
  [(f2 . args) (format "got ~a args" (length args))])

(f2)
(f2 6 7)
(f2 1 2 3 4)
(f2 "three" "arguments" "will-not-trigger-an-error-this-time")

]


@section{Macro}

@defmodule[br/macro #:packages ("beautiful-racket-lib" "beautiful-racket-macro")]



@defform*[
#:literals (syntax lambda stx)
[
(define-macro (id pat-arg ...) result-expr ...+) 
(define-macro id (syntax other-id)) 
(define-macro id (lambda (arg-id) result-expr ...+))
(define-macro id transformer-id)
(define-macro id syntax-object) 
]]
Create a macro using one of the subforms above, which are explained below:

@specsubform[#:literals (define-macro) 
(define-macro (id pat-arg ...) result-expr ...+)]{
If the first argument is a @seclink["stx-patterns" #:doc '(lib "scribblings/reference/reference.scrbl")]{syntax pattern} starting with @racket[id], then create a syntax transformer for this pattern using @racket[result-expr ...] as the return value. As usual, @racket[result-expr ...] needs to return a @seclink["stx-obj" #:doc '(lib "scribblings/guide/guide.scrbl")]{syntax object} or you'll get an error.

The syntax-pattern notation is the same as @racket[syntax-case], with one key difference. If a @racket[pat-arg] has a name written in @tt{CAPS}, it's treated as a named wildcard (meaning, it will match any expression in that position, and can be subsequently referred to by that name). Otherwise, @racket[pat-arg] is treated as a literal (meaning, it will only match the same expression). If @racket[pat-arg] is a literal identifier, it will only match another identifier with the same name @italic{and} the same binding (in other words, identifiers are tested with @racket[free-identifier=?]).

For instance, the @racket[sandwich] macro below requires three arguments, and the third must be @racket[please], but the other two are wildcards:

@examples[#:eval my-eval
(define-macro (sandwich TOPPING FILLING please)
  #'(format "I love ~a with ~a." 'FILLING 'TOPPING))

(sandwich brie ham)
(sandwich brie ham now)
(sandwich brie ham please)
(sandwich banana bacon please)

]

The ellipsis @racket[...] can be used with a wildcard to match a list of arguments. Please note: though a wildcard standing alone must match one argument, once you add an ellipsis, it's allowed to match zero:

@examples[#:eval my-eval
(define-macro (pizza TOPPING ...)
  #'(string-join (cons "Waiter!"
                       (list (format "More ~a!" 'TOPPING) ...))
                 " "))

(pizza mushroom)
(pizza mushroom pepperoni)
(pizza)
]

The capitalization requirement for a wildcard @racket[pat-arg] makes it easy to mix literals and wildcards in one pattern. But it also makes it easy to mistype a pattern and not get the wildcard you were expecting. Below, @racket[bad-squarer] doesn't work because @racket[any-number] is meant to be a wildcard. But it's not in caps, so it's considered a literal, and it triggers an error:

@examples[#:eval my-eval
(define-macro (bad-squarer any-number)
  #'(* any-number any-number))
(bad-squarer +10i)
]

The error is cleared when the argument is in caps, thus making it a wildcard:

@examples[#:eval my-eval
(define-macro (good-squarer ANY-NUMBER)
  #'(* ANY-NUMBER ANY-NUMBER))
(good-squarer +10i)
]

You can use the special variable @racket[caller-stx] — available only within the body of @racket[define-macro] — to access the original input argument to the macro.

@;{todo: fix this example. complains that caller-stx is unbound}
@examples[#:eval my-eval
(define-macro (inspect ARG ...)
  (with-pattern ([CALLER-STX (syntax->datum caller-stx)])
    #`(displayln
       (let ([calling-pattern 'CALLER-STX])
         (format "Called as ~a with ~a args"
                 calling-pattern
                 (length (cdr calling-pattern)))))))
(inspect)
(inspect "foo" "bar")
(inspect #t #f #f #t)
]
}

This subform of @racket[define-macro] is useful for macros that have one calling pattern. To make a macro with multiple calling patterns, see @racket[define-macro-cases].
}


@specsubform[#:literals (define-macro syntax lambda stx) 
(define-macro id (syntax other-id))]{
If the first argument is an identifier @racket[id] and the second a syntaxed identifier that looks like @racket[(syntax other-id)], create a rename transformer, which is a fancy term for ``macro that replaces @racket[id] with @racket[other-id].'' (This subform is equivalent to @racket[make-rename-transformer].)

Why do we need rename transformers? Because an ordinary macro operates on its whole calling expression (which it receives as input) like @racket[(macro-name this-arg that-arg . and-so-on)]. By contrast, a rename transformer operates only on the identifier itself (regardless of where that identifier appears in the code). It's like making one identifier into an alias for another identifier.

Below, notice how the rename transformer, operating in the macro realm, approximates the behavior of a run-time assignment. 

@examples[#:eval my-eval
(define foo 'foo-value)
(define bar foo)
bar
(define-macro zam-macro #'foo)
zam-macro
(define add +)
(add 20 22)
(define-macro sum-macro #'+)
(sum-macro 20 22)
]
}


@specsubform[#:literals (define-macro lambda stx) 
(define-macro id (lambda (arg-id) result-expr ...+))]{
If the first argument is an @racket[id] and the second a single-argument function, create a macro called @racket[id] that uses the function as a syntax transformer. This function must return a @seclink["stx-obj" #:doc '(lib "scribblings/guide/guide.scrbl")]{syntax object}, otherwise you'll trigger an error. Beyond that, the function can do whatever you like. (This subform is equivalent to @racket[define-syntax].)

@examples[#:eval my-eval
(define-macro nice-sum (lambda (stx) #'(+ 2 2)))
nice-sum
(define-macro not-nice (lambda (stx) '(+ 2 2)))
not-nice
]
}

@specsubform[#:literals (define-macro lambda stx) 
(define-macro id transformer-id)]{
Similar to the previous subform, but @racket[transformer-id] holds an existing transformer function. Note that @racket[transformer-id] needs to be visible during compile time (aka @italic{phase 1}), so use @racket[define-for-syntax] or equivalent.

@examples[#:eval my-eval
(define-for-syntax summer-compile-time (lambda (stx) #'(+ 2 2)))
(define-macro nice-summer summer-compile-time)
nice-summer
(define summer-run-time (lambda (stx) #'(+ 2 2)))
(define-macro not-nice-summer summer-run-time)
]
}

@specsubform[#:literals (define-macro) 
(define-macro id syntax-object)
#:contracts ([syntax-object syntax?])]{
If the first argument is an @racket[id] and the second a @racket[syntax-object], create a syntax transformer that returns @racket[syntax-object]. This is just alternate notation for the previous subform, wrapping @racket[syntax-object] inside a function body. The effect is to create a macro from @racket[id] that always returns @racket[syntax-object], regardless of how it's invoked. Not especially useful within programs. Mostly handy for making quick macros at the REPL.

@examples[#:eval my-eval
(define-macro bad-listener #'"what?")
bad-listener
(bad-listener)
(bad-listener "hello")
(bad-listener 1 2 3 4)
]

}




@defform[
(define-macro-cases id 
  [pattern result-expr ...+] ...+) 
]{
Create a macro called @racket[id] with multiple branches, each with a @racket[pattern] on the left and @racket[result-expr]  on the right. The input to the macro is tested against each @racket[pattern]. If it matches, then @racket[result-expr] is evaluated.

As with @racket[define-macro], wildcards in each syntax pattern must be in @tt{CAPS}. Everything else is treated as a literal match, except for the ellipsis @racket[...] and the wildcard @racket[_].

@examples[#:eval my-eval
(define-macro-cases yogurt
  [(yogurt) #'(displayln (format "No toppings? Really?"))]
  [(yogurt TOPPING) 
   #'(displayln (format "Sure, you can have ~a." 'TOPPING))]
  [(yogurt TOPPING ANOTHER-TOPPING ... please)
   #'(displayln (format "Since you asked nicely, you can have ~a toppings." 
   (length '(TOPPING ANOTHER-TOPPING ...))))]
  [(yogurt TOPPING ANOTHER-TOPPING ...)
   #'(displayln (format "Whoa! Rude people only get one topping."))])

(yogurt)
(yogurt granola)
(yogurt coconut almonds hot-fudge brownie-bites please)
(yogurt coconut almonds)
]

}

@defthing[caller-stx syntax?]{
A special variable only available inside the body of @racket[define-macro] or @racket[define-macro-cases]. It contains the whole original syntax object that was passed to the macro.
                     }


@defform[
(define-unhygienic-macro (id pat-arg ...)
  result-expr ...+) 
]{
Like @racket[define-macro], but moves @racket[result-expr] into the lexical context of the calling site. For demonstration purposes only. If you really need to write an unhygienic macro, this is a rather blunt instrument.
}


@section{Syntax}

@defmodule[br/syntax #:packages ("beautiful-racket-lib" "beautiful-racket-macro")]

@defform[(with-pattern ([pattern stx-expr] ...) body ...+)]{
Bind pattern variables within each @racket[pattern] by matching the pattern to its respective @racket[stx-expr]. These pattern variables can be used in later pattern–expression clauses, or in @racket[body]. Uses the same pattern conventions as @racket[define-macro] (i.e., wildcard variables must be in @tt{CAPS}; everything else is treated as a literal).

@examples[#:eval my-eval
(define-macro (m ARG)
  (with-pattern ([(1ST 2ND 3RD) #'ARG]
                 [(LEFT RIGHT) #'2ND])
    #'LEFT))
(m ((1 2) (3 4) (5 6)))
]
}

@defform[(pattern-case stx ([pattern result-expr ...+] ...))]{
Like @racket[case], but for syntax patterns. Attempt to match @racket[stx] to each successive syntax @racket[pattern]. If a match is found, evaluate the @racket[result-expr] on the right. Uses the same pattern conventions as @racket[define-macro] (i.e., wildcard variables must be in @tt{CAPS}; everything else is treated as a literal). If no match is found a syntax error is raised.

@examples[#:eval my-eval
(define (pc stx)
  (pattern-case stx
    [(1ST 2ND 3RD) #'2ND]
    [(LEFT RIGHT) #'LEFT]))
(pc #'(a b c))
(pc #'(x y))
(pc #'(f))
]
}

@defform[(pattern-case-filter stxs ([pattern result-expr ...+] ...))]{
Attempt to match each element of @racket[stxs] (which is either a list of syntax objects, or a syntaxed list) to each successive syntax @racket[pattern]. Uses the same pattern conventions as @racket[define-macro] (i.e., wildcard variables must be in @tt{CAPS}; everything else is treated as a literal).  If a match is found, evaluate the @racket[result-expr] on the right. If @racket[result-expr] is @racket[#f], or no match is found, then the element is skipped. The result is a list of syntax objects.

@examples[#:eval my-eval
(pattern-case-filter #'((a b c) (x y) (f))
  [(1ST 2ND 3RD) #'2ND]
  [(LEFT RIGHT) #'LEFT])
]
}


@defproc[
 (prefix-id
  [prefix (or string? symbol?)] ...
  [id-or-ids (or/c identifier? (listof identifier?))]
  [#:source loc-stx syntax? #f]
  [#:context ctxt-stx syntax? #f])
  (or/c identifier? (listof identifier?))]{
Create a new identifier within the lexical context of @racket[id-or-ids] with the same name, but prefixed with @racket[prefix]. If there's more than one @racket[prefix] argument, they are concatenated. If @racket[id-or-ids] is a single identifier, then the function returns  a single identifier. Likewise, if it's a list of identifiers, the function returns a list of identifiers, all prefixed.

The optional @racket[loc-stx] argument supplies the source location for the resulting identifier (or identifiers).

The optional @racket[ctxt-stx] argument supplies the lexical context for the resulting identifier (or identifiers).

@examples[#:eval my-eval
(define-macro ($-define ID VAL)
  (with-pattern ([PREFIXED-ID (prefix-id '$ #'ID)]) 
    #'(define PREFIXED-ID VAL)))
($-define foo 42)
$foo
]
}

@defproc[
(suffix-id
  [id-or-ids (or/c identifier? (listof identifier?))]
  [suffix (or string? symbol?)] ...
  [#:source loc-stx syntax? #f]
  [#:context ctxt-stx syntax? #f])
  (or/c identifier? (listof identifier?))]{
Create a new identifier within the lexical context of @racket[id-or-ids] with the same name, but suffixed with @racket[suffix]. If there's more than one @racket[suffix] argument, they are concatenated. If @racket[id-or-ids] is a single identifier, then the function returns  a single identifier. Likewise, if it's a list of identifiers, the function returns a list of identifiers, all suffixed.

The optional @racket[loc-stx] argument supplies the source location for the resulting identifier (or identifiers).

The optional @racket[ctxt-stx] argument supplies the lexical context for the resulting identifier (or identifiers).

@examples[#:eval my-eval
(define-macro (define-% ID VAL)
  (with-pattern ([ID-SUFFIXED (suffix-id #'ID '%)]) 
    #'(define ID-SUFFIXED VAL)))
(define-% foo 42)
foo%
]
}

@defproc[
(infix-id
  [prefix (or string? symbol?)]
  [id-or-ids (or/c identifier? (listof identifier?))]
  [suffix (or string? symbol?)] ...
  [#:source loc-stx syntax? #f]
  [#:context ctxt-stx syntax? #f])
  (or/c identifier? (listof identifier?))]{
Create a new identifier within the lexical context of @racket[id-or-ids] with the same name, but prefixed with @racket[prefix] and suffixed with @racket[suffix]. If there's more than one @racket[suffix] argument, they are concatenated. If @racket[id-or-ids] is a single identifier, then the function returns  a single identifier. Likewise, if it's a list of identifiers, the function returns a list of identifiers, all suffixed.

The optional @racket[loc-stx] argument supplies the source location for the resulting identifier (or identifiers).

The optional @racket[ctxt-stx] argument supplies the lexical context for the resulting identifier (or identifiers).

@examples[#:eval my-eval
(define-macro ($-define-% ID VAL)
  (with-pattern ([ID-INFIXED (infix-id '$ #'ID '%)]) 
    #'(define ID-INFIXED VAL)))
($-define-% foo 42)
$foo%
]
}


@(require (for-label syntax/strip-context))
@defproc[(strip-bindings [stx syntax?]) syntax?]{

Removes all bindings from @racket[stx], but preserves
source-location information and properties. An alias for @racket[strip-context].}

@defproc[(replace-bindings [stx-source (or/c syntax? #f)] [stx-target syntax?]) syntax?]{

Uses the bindings from @racket[stx-source] to replace the bindings of all parts of @racket[stx-target], while preserving source-location
information and properties. An alias for @racket[replace-context].}


@defproc[
 (stx-flatten [stx syntax?])
  (listof syntax?)]{

Converts a hierarchical syntax object into a flat list of component syntax objects, discarding all the intervening structure. Useful when you need to get at the ``bottommost'' syntax objects.

@examples[#:eval my-eval
(define my-stx #'(let ([x 42]
                    [y 25])
                (define (f z) (* x y z))
                (displayln (f 11))))
(map syntax->datum (stx-flatten my-stx))
]

}

@section{Indentation}

@defmodule[br/indent]

Helper functions for DrRacket language indenters.

@defproc[(char 
[textbox (is-a?/c text%)]
[position (or/c exact-nonnegative-integer? #f)])
(or/c char? #f)]{
Get the character in @racket[textbox] that lives at @racket[position].

@;{
can't get this example to work without racket/gui/base instantiation error
@examples[#:eval my-eval
(define tb (new text%))
(send tb insert "foobar")
(char tb 4)
]
}

}


@defproc[(line 
[textbox (is-a?/c text%)]
[position (or/c exact-nonnegative-integer? #f)])
exact-nonnegative-integer?]{
Get the line index in @racket[textbox] that contains @racket[position].
}

@defproc[(previous-line 
[textbox (is-a?/c text%)]
[position (or/c exact-nonnegative-integer? #f)])
(or/c exact-nonnegative-integer? #f)]{
Get the line index in @racket[textbox] of the line before the one that contains @racket[position].
}

@defproc[(next-line 
[textbox (is-a?/c text%)]
[position (or/c exact-nonnegative-integer? #f)])
(or/c exact-nonnegative-integer? #f)]{
Get the line index in @racket[textbox] of the line after the one that contains @racket[position].
}

@defproc[(line-chars 
[textbox (is-a?/c text%)]
[line-idx (or/c exact-nonnegative-integer? #f)])
(or/c (listof char?) #f)]{
Get the chars in @racket[textbox] on line @racket[line-idx].
}

@defproc[(line-start 
[textbox (is-a?/c text%)]
[line-idx (or/c exact-nonnegative-integer? #f)])
(or/c exact-nonnegative-integer? #f)]{
Get the starting character position in @racket[textbox] of line @racket[line-idx] (or @racket[#f] if there is no such line). To get the actual character, pass the return value to @racket[char].
}



@defproc[(line-end
[textbox (is-a?/c text%)]
[line-idx (or/c exact-nonnegative-integer? #f)])
(or/c exact-nonnegative-integer? #f)]{
Get the ending character position in @racket[textbox] of line @racket[line-idx] (or @racket[#f] if there is no such line). To get the actual character, pass the return value to @racket[char].
}

@deftogether[(
@defproc[(line-start-visible 
[textbox (is-a?/c text%)]
[line-idx (or/c exact-nonnegative-integer? #f)])
(or/c exact-nonnegative-integer? #f)]
@defproc[(line-end-visible 
[textbox (is-a?/c text%)]
[line-idx (or/c exact-nonnegative-integer? #f)])
(or/c exact-nonnegative-integer? #f)]
)]{
Like @racket[line-start] and @racket[line-end], but skips whitespace characters. To get the actual character, pass the return value to @racket[char], or use @racket[line-first-visible-char] and @racket[line-last-visible-char].
}

@defproc[(line-first-visible-char 
[textbox (is-a?/c text%)]
[line-idx (or/c exact-nonnegative-integer? #f)])
(or/c char? #f)]{
Convenient notation for @racket[(char textbox (line-start-visible textbox line-idx))].
}

@defproc[(line-last-visible-char 
[textbox (is-a?/c text%)]
[line-idx (or/c exact-nonnegative-integer? #f)])
(or/c char? #f)]{
Convenient notation for @racket[(char textbox (line-end-visible textbox line-idx))].
}

@defproc[(line-indent
[textbox (is-a?/c text%)]
[line-idx (or/c exact-nonnegative-integer? #f)])
(or/c exact-nonnegative-integer? #f)]{
Get the length of the indent of line @racket[line-idx] in @racket[textbox].
}


@defproc[(apply-indenter
[indenter-proc procedure?]
[textbox-or-str (or/c (is-a?/c text%) string?)])
string?]{
Apply @racket[indenter-proc] to the text in @racket[textbox-or-str] and return an indented string. Useful for unit testing.
}


@defproc[(string-indents
[str string?])
(listof exact-nonnegative-integer?)]{
Lists the indents at the beginning of each line in @racket[str]. Useful for unit testing.
}


@section{Lists}

@defmodule[br/list]

@defform[(values->list values)]{
Convert @racket[values] to a simple list.

@examples[#:eval my-eval
(split-at '(a b c d e f) 3)
(values->list (split-at '(a b c d e f) 3))
]
}

@defform[(push! list-id val)]{
Mutate @racket[list-id] by putting @racket[val] on top.

@examples[#:eval my-eval
(define xs '(2 3 4))
(push! xs 1)
xs
]
}

@defform[(pop! list-id)]{
Mutate @racket[list-id] by removing the topmost element, and return this element as the result.

@examples[#:eval my-eval
(define xs '(1 2 3 4))
(define top (pop! xs))
top
xs
]
}

@section{Reader utilities}

@defmodule[br/reader-utils]


@defproc[
(apply-reader
[read-syntax-proc procedure?]
[source-str string?])
datum?]{
Applies @racket[read-syntax-proc] to @racket[source-str] as if it were being read in from a source file. Perhaps helpful for testing & debugging.
}


@section{The @tt{br} teaching languages}

@defmodulelang*[(br
br/quicklang)]{
The @racketmodname[br] language is designed to be used for the samples and tutorials in @italic{Beautiful Racket}. The language is a convenience variant of @racketmodname[racket/base] that includes the modules listed above, plus a number of other Racket libraries. The specifics are not hugely relevant. Why not? Because in the future, @racketmodname[br] will grow depending on the needs of the @italic{Beautiful Racket} book. Therefore, it's probably not what you want to use for your own projects. See the @link["http://beautifulracket.com/appendix/from-br-to-racket-base.html"]{appendix of @italic{Beautiful Racket}} for more information.

@racketmodname[br/quicklang] is a variant of the @racketmodname[br] language that automatically exports @racket[#%top], @racket[#%app], @racket[#%datum], and @racket[#%top-interaction]. As above, if you're making your own language, you should build on @racketmodname[racket/base], because @racketmodname[br/quicklang] does nothing that you couldn't do yourself in two lines of code.

}

