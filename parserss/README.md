# Parser

## Rediscover Parser, First Principles P/915

P/916

using: <https://github.com/ekmett/trifecta/>

> The basic idea behind a parser is that you're
> moving a sort of cursor around a linear stream of text. It's simplest
> to think of the individual units within the stream as characters or
> ideographs, though you'll want to start thinking of your parsing
> problems in chunkier terms as you progress.

P/916

> One of the hardest problems in writing parsers, especially the
> parser libraries themselves, is making it easy to express things
> the way the programmer would like, but still have the resulting
> parser be fast

`>>`: value from the previous computation `m a` gets thrown away, but any
efffect the `m a` action **had upon the monadic context remains**

P/963

> the act of parsing, in a sense, is a means of "necking down" the
> cardinality of our inputs to the set of things our programs have a
> sensible answer for.

(recall the design pattern: parse-dont-validate, the two-phase parsing)

> you might have a sort of AST or structured representation of what was
> parsed, but from there, you might expect that AST or representation to
> take a particular form. This means we want to narrow the cardinality
> and get even more specific about how our data looks.
> Often this second step is called `unmarshalling our data`
> Similarly, `marshalling` is the act of preparing data for serialization

### A refresher on State

> any put to the State value would be observable to the next action
> in the same Monad

put() returns a unit value, a throwaway value, so we're only
evaluating it for effect anyway; it modifies the state but doesn't
have any value of its own.

```haskell
hs> :set -package transformers-0.5.6.2
package flags have changed, resetting and loading new packages...
hs> import Control.Monad.Trans.State

hs> runStateT (put 8) 7
((),8)

hs> runStateT (put 8 >> get) 7
(8,8)
hs> runStateT (put 1 >> get) 7
(1,1)
hs> runStateT (put 1 >> (return 9001)) 7
(9001,1)
```

### Typeclass Parsing has Alternative as a superclass

P/932

```haskell
class Alternative m => Parsing m where
    try :: m a -> m a
```

this takes a parser that may consume input and, on failure, goes
back to where it started ad fails if we didn't consume input

> we can read `<|>` as being an `or` or disjunction, of our two parsers,
> `many` is `zero or more` and `some` is `one or more`

(recall the hand-rolled conjunction operator `<&&>`)

### INI-Parser

P/952

Note: for a production-ready INI-Parser, see `hsDataMunging/ini-config`,
which uses the `ini` package;

testing the parser is mainly based on HSpec; create a "Result to Maybe"
converter in order to inspect (and to create the expected value for) the
output from `parseByteString`

### Character and Token Parser

P954

> Traditionally, parsing has been done in two stages, lexing and parsing.
> Characters from a stream will be fed into the lexer, which will then emit
> tokens on demand to the parser until it has no more to emit.
> The parser then structures the stream of tokens into a tree, commonly called
> an "abstract syntax tree" or AST

P/955

> Lexers are simpler, typically performing parses that don't require looking
> ahead into the input stream by more than one character or token at a time.
> Lexers are at times called tokenizers.

P/957

> tokenization isn't exclusively about whitespace, it's about ignoring noise
> so you can focus on the structures you are parsing.

beware of the non-backtracking parser library such as autoparse;
use `try` to reset the cursor

### Unmarshalling: from AST to a datatype

P/963

> It's extremely unlikely you can do something meaningful and domain-specific
> when your input type is String, Text, or ByteString. However, if you can
> parse one of those types into something structured, rejecting bad inputs,
> then you might be able to write a proper program.

don't play with text; play with structure

> One of the mistakes programmers make in writing programs handling text
> is in allowing their data to stay in the textual format, doing mind-bending
> backflips to cope with the unstructured nature of textual inputs.

the following guide rings many bells:

> In some cases, the act of parsing isn't enough. You might have a sort
> of AST or structured representation of what was parsed, but from there,
> you might expect that AST or representation to take a particular form.
> This means we want to narrow the cardinality and get even more specific
> about how our data looks. Often this second step is called unmarshalling our data.

(recall the "shellto" project that had undergone a
strategic shift, from emitting code from AST directly to unmarshalling the
AST to an IR, and then emitting code)

> Similarly, marshalling is the act of preparing data for serialization,
> whether via memory alone (foreign function interface boundary) or over a network interface.

```haskell
Text -> Structure -> Meaning
-- parse -> unmarshall

Meaning -> Structure -> Text
-- marshall -> serialize
```
