Chain operator for Erlang
=========================

The main goal of this parse transform is to make the Erlang syntax evem more
evil.

**License**: MIT

**Author**: Uvarov Michael (freeakk@gmail.com)


.. image:: https://secure.travis-ci.org/freeakk/chacha.png?branch=master
    :alt: Build Status
    :target: http://travis-ci.org/freeakk/chacha


Syntax
======

f(g(x)) is

- in mathematics : f ∘ g (x)
- in haskell : ( f . g ) (x)
- in erlang: f(g(X)).
- here: chain + f + g ++ X.


- in erlang: ``[_, X|_] = Xs.``
- in erlang (again): ``head(tail(Xs)).``
- here: ``chain + head + tail ++ Xs.``


Creating a higher-order function (HOF).

- in erlang: ``F = fun([_, X|_]) -> X end.``
- here: ``F = chain * 1 + head + tail.``
  where 1 is arity of the function.


Sum example
-----------

In Erlang::

    sum() -> fun(Xs) -> lists:foldl(fun erlang:'+'/2, 0, Xs) end.

Here::

    sum() -> chain * 1 + lists:foldl('+' ++ 0).

Or::
    sum() -> chain * 1 + lists:foldl('+', _) ++ 0.

Or::
    sum() -> chain * 1 + 1 * lists:foldl('+') ++ 0.
