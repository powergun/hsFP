# MoveCursor implemented using State

NOTE:

re video | monad transformer state

StateT is good at dealing with "small problem" but it is discouraged
in large scale project, particularly when multi-threading and concurrency
is involved, as the general principle in Haskell says, stay with
immutablity

ReaderT (and ReaderT WriterT transformer stack) may look awkward
but read-only is a beauty
