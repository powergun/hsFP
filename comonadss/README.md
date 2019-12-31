# comonad

source: <https://bartoszmilewski.com/2017/01/02/comonads/>

## Monad & Comonad

Just as a Kleisli arrow takes a value and produces some embellished result — it embellishes it with context — a co-Kleisli arrow takes a value together with a whole context and produces a result. It’s an embodiment of contextual computation.

### Monad

> A monad provides a way of putting a value in a container using
> return. It doesn’t give you access to a value or values stored inside.
> we’ve seen the example of the IO monad that prides itself in never exposing its contents.

### Comonad

> provides the means of extracting a single value from it. It does
> not give the means to insert values. So if you want to think of a
> comonad as a container, it always comes pre-filled with contents,
> and it lets you peek at it.

## The "Product" comonad

inspired by the reader monad; to return the `a` from the monad

see: src/TheProduct.hs

> In a way, the comonadic implementation of the environment is more
> natural — it follows the spirit of “computation in context.” On the
> other hand, monads come with the convenient syntactic sugar of the do notation.

see: src/TheComonad.hs and src/ComonadLib.hs for demos using a from-scratch
implementation and the public package comonad library

## The Stream Comonad

> This process of shifting the focus from one element of the container to another is best illustrated with the example of an infinite stream

see src/TheStream.hs
