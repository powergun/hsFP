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
