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
<https://ro-che.info/articles/2015-05-02-smarter-validation>
<https://medium.com/blacklane-engineering/pure-functional-validation-64a7885d22ac>
-> https://www.parsonsmatt.org/2017/10/11/type_safety_back_and_forth.html
-> https://kataskeue.com/gdp.pdf
<https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/>
