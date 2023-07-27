# MSet
Implementation of a type constructor for [multisets](https://en.wikipedia.org/wiki/Multiset), with an empty constructor and the common operators for working on multisets.

This implementation is an instance of the type classes `Eq` and `Foldable` and provides an operator `mapMSet` that emulates `fmap`, but impose an additional constraint on the output type, so it is not a `Functor`.

The test file exploits _IO Monads_ for reading and writing files, computing the count of the _C.I.A.O._ (Characters In Alphabetical Order) of each word.
