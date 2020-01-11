# Parser

## Rediscover Parser, First Principles P/915

P/916

using: <https://github.com/ekmett/trifecta/>

> The basic idea behind a parser is that you're
> moving a sort of cursor around a linear stream of text. It's simplest
> to think of the individual units within the stream as characters or
> ideographs, though you'll want to start thinking of your parsing
> problems in chunkier terms as you progress.

P/916

> One of the hardest problems in writing parsers, especially the
> parser libraries themselves, is making it easy to express things
> the way the programmer would like, but still have the resulting
> parser be fast

`>>`: value from the previous computation `m a` gets thrown away, but any
efffect the `m a` action **had upon the monadic context remains**
