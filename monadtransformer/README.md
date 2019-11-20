# Monad Transformer

source:

https://en.wikibooks.org/wiki/Haskell/Monad_transformers

haskell cookbook (web version) chapter 18

## Terminology

we will use **precursor monad** to refer to the non-transformer (e.g.
Maybe in MaybeT stack), on which a transformer is based;

**base monad** is the other monad (e.g. IO in MaybeT IO stack)
on which the transformer is applied

## Simple Transformer

[SimpleTransformer (haskell cookbook)](src/SimpleTransformer)

## Directory Walker

progressively improve a naive, non-Mt implementation to use Mt

[Directory walker (real world haskell)](src/DirWalker)

## Transformer Stack

the concept of using multiple Mt

[TransformerStack: tooling for the stack](src/TransformerStack)

## MaybeT

develop MaybeT from scratch and see how it dramatically simplifies
real-world use case that relies on early termination - the passphrase problem.

[Transformer from scratch: The Maybe Transformer](src/MaybeTrans)

