# applicative

source:

<https://mmhaskell.com/monads/applicatives>

## ParserMonad

a monadic parsing example; it implements a mini-parsec from scratch

## When functor can not do the job (Monady morning haskell)

However, what happens when we try to combine wrapped data?
For instance, if we try to have GHCI interpret these calculations,
we’ll get type errors:

```haskell
>> (Just 4) * (Just 5)
>> Nothing * (Just 2)
```

## Parallel application functions (Monday morning haskell)

**MY NOTE**: this is touched in haskell class

```haskell
-- see homework 03 reference
-- $ zip3 l (drop 1 l) (drop 2 $ l) l
-- . (zip3 <$> id <*> drop 1 <*> drop 2)
. (zip3 <*> drop 1 <*> drop 2)

-- id can be dropped from the definition because of functor law

-- λ> f = zip3 <$> id
-- λ> :t f
-- f :: [a] -> [b] -> [c] -> [(a, b, c)]
-- λ> :t zip3
-- zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]

-- https://mmhaskell.com/monads/laws:
-- fmap id `is` fmap
```

You might be wondering how we might do parallel application of functions.
For instance, we might want to use the second list example above, but
have the result be [2,10,30]. There is a construct for this, called
`ZipList` ! It is a newtype around list, whose Applicative instance
is **designed to use this behavior.**

```haskell
>> ZipList [(1+), (5*), (10*)] <*> [5,10,15]
ZipList {getZipList = [6,50,150]}
```

## The basics of Applicative Functors, Put to practical work

source: <http://www.serpentine.com/blog/2008/02/06/the-basics-of-applicative-functors-put-to-practical-work/>

> I won't attempt to describe what applicative functors actually
> are, because the idea is easy to absorb: we will pick it up as
> an incidental product of figuring out how to use the,

## Use applicative validation to check JSON data (good article)

### Use newtype to re-define the behavior of Either

see: Validation/Newtype.hs

source: <https://blog.ploeh.dk/2018/11/05/applicative-validation/>

> My motivation for introducing a new type is that the way that Either is Applicative is not quite how I'd like it to be. Introducing a newtype enables you to change how a type behaves

as the article points out:

> That's the reason you can't use Either. While it's Applicative, it doesn't behave like you'd like it to behave in this scenario. Particularly, the problem is that it throws away all but the first Left value it finds

#### MY NOTES

run this experiment in ghci: `Right (:) <*> Left 32 <*> Left 1`; the result is Left 32

if I don't care the Right value, i.e. I just want to validate the input
value using a series of rules, use the "point to" applicative operator:
`<*` - this also appears in the Validation package example below:

### the Validation package (data61)

source: <https://github.com/qfpl/validation/blob/master/examples/src/Email.hs>

it defines the instance for other popular typeclasses, such as foldable

### other

<https://codurance.com/2017/11/30/applicatives-validation/>
<https://codurance.com/2018/01/11/applicatives-validation/>

<https://haskell-at-work.com/episodes/2018-02-26-validation-with-smart-constructors.html>

MY NOTES: this is really just a variation of the "factory pattern"

<https://ro-che.info/articles/2015-05-02-smarter-validation>
<https://medium.com/blacklane-engineering/pure-functional-validation-64a7885d22ac>
-> <https://www.parsonsmatt.org/2017/10/11/type_safety_back_and_forth.html>
-> <https://kataskeue.com/gdp.pdf>
<https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/>

## Implement Singleton Pattern using Applicative

source: <https://github.com/thma/LtuPatternFactory>

see: Singleton/Env.hs

see also: comonad/TheProduct - inspired by the "reader monad"; note
how the shared (singleton) state is threaded in various compute()
functions (and affect their behavior)

## Rediscover Applicative (First Principles)

source: first principles P/704

MY NOTES: follow the foot step of ../functors/"Rediscover Functor";
note that functor's fmap can not satisfy the computation that involves
two (or more) operands: `fmap (+) [1] [2]`; recall that binary operation is
curried to a unary operation (a function) then applied with the second arg/lhs;
functor does not have the structure to hold a function - recall the
saying, that applicative has more structure than functor

> So, with Applicative, we have a Monoid for our structure and function application for our values!

### Re-explain: `f <$> g <*> h` and `f <$> g <*> h $ arg`

P/715;

> So we first fmap those functions over the value inside the first Maybe context, if it’s a Just value, making it a partially applied function wrapped in a Maybe context. Then we use the tie-fighter to apply that to the second value, again wrapped in a Maybe. If either value is a Nothing, we get Nothing.
> (P/869) we'd use this, when two functions would share the same input
> and we want to apply some other function to the result of those to reach
> a final result

see also: P/867; Note that one should read this expression **left to right**; this is called **parallel-application**; the left-to-right evaluation can be
explained as:

`(+) <$> (* 2)`

> mapping a function awaiting two args over a function awaiting one
> produces a two arg function; this is identical to function-composition
> `((+)) . (* 2) == \x -> (+) (2 * x)`
> the tricky part is that even after we apply `x`, we've got (+) partially
> applied to the first argument which was doubled by `(* 2)`

`(+) <$> (* 2) <*> (+ 10)`

`+ 10` is `f`, `((->) a)` as in `f (a -> b) -> f a -> f b`; while the
first bit is still the two-arg function:

```haskell
λ> :t (<*>) :: (a -> a -> b) -> (a -> a) -> (a -> b)
(<*>) :: (a -> a -> b) -> (a -> a) -> (a -> b)
  :: (a -> a -> b) -> (a -> a) -> a -> b
```

> we are feeding a single argument to the `(*2)` and `(+10)` and the
> two results form the two arguments to `(+)`

see also: P/874

> we can determine that `r`, the argument type for functions, is part
> of the structure being `lifted over` when we lift over a function,
> not the value being transformed or mapped over
> this leaves the result of the function as the value being transformed.
> this happens to line up neatly with what function composition is about

#### how to understand `f <$> g <*> h $ arg`

example:

```haskell
t = (,) <$> (+ 1) <*> (+ 10)

:t t
t :: Num a => a -> (a, a)

t 1
(2,11)
```

g, h has the same structure `a -> a` - which is a functorial structure
rather than a data structure - therefore the resulting value must also
follow the same functorial structure, meaning that it must also be a
function of `a -> a`; the applicative operation here is to combine
the functorial quality (i.e. function application) as well as the monoidal
quality (i.e. ensure the resulting structure)

### homomorphism

> A homomorphism is a structure-preserving map between two algebraic structures. The effect of applying a function that is embedded in some structure to a value that is embedded in some structure should be the same as applying a function to a value without affecting any outside structure
> The general idea of the homomorphism law is that applying the function doesn’t change the structure around the values.

examples:

```haskell
λ> pure (+1) <*> pure 1
2
λ> pure ((+1) 1)
2
λ> pure 1
1
λ> pure (+1) <*> pure 1 :: Maybe Int
Just 2
λ> pure (+1) <*> pure 1 :: [Int]
[2]
```

### Use QuickCheck to test law compliance

see: test/DemoQuickCheckLaw.hs; source: first principles P/739

### Discard Applicative Result

P/809

> Basically the `(<*)` operator (like its sibling, `(*>)`, and the monadic operator, `>>`) is useful when you’re emitting effects

```haskell
λ> (,) <$> [2] <*> [3]
[(2,3)]
λ> (,) <$> [2] <*> [3] <* [123]
[(2,3)]
```

Note how the result of `[123]` is ignored

```haskell
λ> (,) <$> Just 1 <*> Just 123
Just (1,123)
λ> (,) <$> Just 1 <*> Just 123 <* Nothing
Nothing
```

**Note how `<* Nothing` does not participate in `(,)` but has impact
to the end value (Nothing instead of Just)**

See the source code for the implemention: <http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html#line-841>

```haskell
λ> -- Just _m1 *> m2 = m2
λ> -- Nothing *> _m2 = Nothing
```

### Use LiftA2 to compose boolean operations

source: First Principles P/811; see FirstPrinciples/LiftA2ForBoolean.hs

> It’s parallel application of the functions against an argument.

see above: how to understand `f <$> g <*> h $ arg`;
also recall: **Function is Structure**

### Explain Applicative using IO

see: `io-sinbin`; emphasis on the both effects (of f and arg)
