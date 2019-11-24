# alternative

## Alternative

source:

https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus

## MonadPlus

source:

https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus

One might legitimately wonder why the seemingly redundant MonadPlus
class exists. Part of the reason is historical: just like Monad existed
in Haskell long before Applicative was introduced, MonadPlus is much
older than Alternative. Beyond such accidents, there are additional
expectations (ones that do not apply to Alternative) about how
the MonadPlus methods should interact with the Monad, and therefore
indicating that something is a MonadPlus is a stronger claim than
indicating that it is both an Alternative and a Monad.
