{-
# x = 1 + 2
# y = x + 1

seq y ()

seq function evaluates its first argument, y, then returns its second
argument (just a ()),

para con haskell L350
the seq() function evaluates its argument only as far as the first
ctor, and does not eval any more of the structure
there is a technical term for this:
we say that seq() eval its first argument to weak head normal form
the term normal head on its own means: fully evaluated


-}
