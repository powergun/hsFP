# Foldables

## Discovery Foldables - First Principles

source: First Principles P/829

> class of data structures that can be folded to a summary value.

`class Foldable (t :: * -> *) where`

> That t should be a higher-kinded type is not surprising:
> lists are higher-kinded types. We need t to be a type constructor for the same reasons we did with Functor, and we will see that the effects are very similar.
> Types that take more than one type argument, such as tuples and Either, will necessarily have their first type argument included as part of their structure.

## What is "Catamorphism"

source: <https://wiki.haskell.org/Catamorphisms>

> Catamorphisms are generalizations of the concept of a fold in functional programming. A catamorphism **deconstructs** a data structure with an F-algebra for its underlying functor.

see also: P/376

> "Cata-" means "down" or "against", as in "catacombs." Catamorphisms are a means of
> deconstructing data.
> If the spine of a list is the structure of a list, then a fold is what can reduce
> that structure

P/835

> we need to apply the folding function to the value and, again, **dispose of the structure**
