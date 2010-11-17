Causal trees in Haskell
==========

Causal trees, [as described by Victor Grishchenko in this paper](http://bouillon.math.usu.ru/articles/ctre.pdf), are a way of representing a Unicode document such that insertion and deletion patches can be applied in any order, and converge to the same result. This is similar to various [operational transformation](http://en.wikipedia.org/wiki/Operational_transformation) schemes, but simpler, and easier to guarantee correctness.

This library contains efficient algorithms and data structures for working with causal trees. It's not ready for release yet.
