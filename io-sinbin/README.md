# IO the "sin bin"

## Re-discover IO, First Principles P/1156

P/1156

> The important thing about IO is that it's a special kind of datatype
> that inherently disallows certain types of sharing, thus preventing the
> effects of those functions from percolating throughout your program.

P/1157

> The IO Monad is just an instance of the ST monad, where the state is
> the real world.
> The issue with this explanation is that you don't usefully see or interact
> with the underlying State# in IO
> it's not the state in the sense that one uses State, StateT or even ST
> although the behavior of `s` is certainly very like that of ST
> The State here is a signalling mechanism for telling GHC what order your
> IO actions are in and what a unique IO action is
> The state tokens underlying the IO type are erased during compile time
> and so add no overhead to your runtime.

the return type of IO, if any

P/1160

> with the `IO` type, you're not guaranteed anything. Values of type `IO a`
> are not an a; they’re a description of how you might get an a. A function
> of type `IO String` is not a computation that, if evaluated, will result in
> a `String`; it’s a description of how you might get that String from the
> "real world", possibly performing effects along the way.

### IO and Sharing

P/1161

see: `src/IOAndSharing.hs`

> The `MVar` type is a means of synchronizing shared data in Haskell.
> the `MVar` can hold one value at a time. You put a value into it; it
> holds onto it until you take that value out; Then and only then can you
> put another value in the Box

P/1164

In the following example, we'll use `Debug.Trace` again to show us when
things are being shared. For `blah`, the trace is outside the IO action, so we'll use outer trace

> We only saw inner and outer emitted once because IO is not intended to
> disable sharing for values not in IO that happen to be used in the course
> of running of an IO action

### Referential Transparency

> Referential transparency is something you are probably familiar with,
> even if you've never called it that before. Put casually, it means that
> any function, when given the same inputs, returns the same result

i.e. the function can be replaced by a lookup table.
(recall: `notes/dont_fear_monad.txt`)

### IO's functor, applicative and monad

P/1167

> IO is a datatype that has a Monad instance — as well as Functor and
> Applicative instances

#### Explaining functor and applicative using IO

`fmap`: construct an action which performs the **same effects** but transforms the `a` into a `b`

`fmap :: (a -> b) -> IO a -> IO b`

`(<*>)`: construct an action that performs **the effects of both the
function and value arguments**, applying the function to the value

`(<*>) :: IO (a -> b) -> IO a -> IO b`

`join`: **merge the effects of a nested** IO action

`join :: IO (IO a) -> IO a`
