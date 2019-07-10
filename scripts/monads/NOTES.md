# Monad notes

## Monad in imperative (OOP) languages

read (is Python with statement a Monad ?):

https://stackoverflow.com/questions/7131027/is-pythons-with-monadic

summary:

Monads, at their most basic level, are more or less a cool way to use continuation-passing style: >>= takes a "producer" and a "callback"; this is also basically what with is: a producer like open(...) and a block of code to be called once it's created.

read (A failed attempt to implement Monad in Python):

https://rbtcollins.wordpress.com/2018/08/26/monads-and-python/

summary:

So there you have it, a three year old mull: perhaps we shouldn’t port Monads to Python at all, and instead just:

    Write pure code (SRP)
    Use a strategy object to represent impure activity (Lizkov)
    Use exceptions to handle short circuiting of code

MY NOTES:

SOLID principle still rules; I need a more high-level, abstract
SOLID principle to guide me through the FP land

read (Applicative/Monad in C++ Boost library):

https://www.boost.org/doc/libs/1_65_0/libs/hana/doc/html/group__group-Monad.html

Summary:

Not quite useful to understand Monad. But this article mentions: 
`monad tutorial fallacy`

https://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/

 If you ever find yourself frustrated and astounded that someone else does not grasp a concept as easily and intuitively as you do, even after you clearly explain your intuition to them (“look, it’s really quite simple,” you say…) then you are suffering from the monad tutorial fallacy.

