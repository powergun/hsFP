# comonad

source: <https://bartoszmilewski.com/2017/01/02/comonads/>

## Monad & Comonad

### Monad

> A monad provides a way of putting a value in a container using
> return. It doesn’t give you access to a value or values stored inside.
> we’ve seen the example of the IO monad that prides itself in never exposing its contents.

### Comonad

> provides the means of extracting a single value from it. It does
> not give the means to insert values. So if you want to think of a
> comonad as a container, it always comes pre-filled with contents,
> and it lets you peek at it.
