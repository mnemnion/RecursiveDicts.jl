# RecursiveDicts

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://mnemnion.github.io/RecursiveDicts.jl/stable/)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://mnemnion.github.io/RecursiveDicts.jl/dev/)
[![Build Status](https://github.com/mnemnion/RecursiveDicts.jl/actions/workflows/CI.yml/badge.svg?branch=trunk)](https://github.com/mnemnion/RecursiveDicts.jl/actions/workflows/CI.yml?query=branch%3Atrunk)
[![Aqua](https://raw.githubusercontent.com/JuliaTesting/Aqua.jl/master/badge.svg)](https://github.com/JuliaTesting/Aqua.jl)

A `RecursiveDict` is a wrapped `Dict`, where values may always be a `Dict` (or
`RecursiveDict`) with the same type parameters.

```jldoctest
julia> rdict = RecursiveDict(:a => 1, :b =>2)
RecursiveDict{Symbol, Int64} with 2 entries:
  :a => 1
  :b => 2

julia> rdict[:c] = Dict(:d => 5, :e => 20)
Dict{Symbol, Int64} with 2 entries:
  :d => 5
  :e => 20

julia> rdict
RecursiveDict{Symbol, Int64} with 3 entries:
  :a => 1
  :b => 2
  :c => RecursiveDict{Symbol, Int64}(:d=>5, :e=>20)

julia> rdict[:f] = Dict("str" => 7)
MethodError: Cannot `convert` an object of type
  RecursiveDict{String, Int64} to an object of type
  Union{Int64, RecursiveDict{Symbol, Int64}}
[...]
```

## Why Does the Type Signature Look Like That?

Type parameters express the free variables in the type. In a `Dict{K,V}`, the keytype and
valtype are both free, so `K` and `V` are both the keytype and valtype, and the free parameters.

```jldoctest
julia> d = Dict{Int,String}(1 => "one", 2 => "two")
Dict{Int64, String} with 2 entries:
  2 => "two"
  1 => "one"

julia> typeof(d)
Dict{Int64, String}

julia> keytype(d)
Int64

julia> valtype(d)
String
```

With a `RecursiveDict`, the free parameters are the `keytype`, and whatever value type
_other than that sort of RecursiveDict_ is allowed, so the above identity doesn't hold.

```jldoctest
julia> rd = RecursiveDict{Int,String}(1 => "one", 2 => "two")
RecursiveDict{Int64, String}) with 2 entries:
  2 => "two"
  1 => "one"

julia> typeof(rd)
RecursiveDict{Int64, String}

julia> keytype(rd)
Int64

julia> valtype(rd)
Union{String, RecursiveDict{Int64, String}}
```

This fact is why `RecursiveDicts` exists, since it makes it impossible to specify
a recursive container without wrapping it in a struct.

## What's the Point?

Types are good for a few things, one of those things is correctness: types can ensure
that certain invariants always hold.  A recursively-defined hashmap is a useful
primitive for building tree structures, for instance, one might make a prefix tree (Trie)
from `RecursiveDict{String,String}`.  It's also the natural structure to build syntax
trees, and many other common composite data types.

The `RecursiveDict` struct is nothing more than an immutable container around a `Dict`.
The Julia compiler is very good at inferring through this kind of wrapper and inlining
methods which act on it, so there should be no performance impact from using it.

It's possible that the compiler might be free to write more efficient code under some
circumstances, although I wouldn't count on it.

The real reason this package exists is because, early in my Julia journey, I drove myself
insane trying to write a type signature for a recursively defined `Dict`.  I learned a lot
about type parameters from the exercise, but had to give up, because it is not, in fact,
possible.

Recursively-defined structs, on the other hand, are legal. Hence this package.

## Should I Use It?

If you want a `Dict` where the `valtype` includes the same sort of Dict, absolutely!
`RecursiveDicts` has a version of every test on `Dict` from Julia core, modified where
necessary to reflect the semantic differences.  Other than the obvious ones involving
`valtype`, `RecursiveDicts` will not promote numeric types which differ to a common type
(with the exception of Pairs passed during the original creation of the dict).  That
might be a bit surprising, but it's consistent with the `valtype` being a Union, Julia
throws an error on code like `convert(Union{Int,Symbol}, 4.0)`.

`convert(Dict, myrecursivedict)` will return the wrapped `Dict`, which will continue to
share contents with the original, and `convert(RecursiveDict, dict)` is also defined,
this makes it mostly painless to use a `RecursiveDict` where ever a `Dict` is
expected.  Many, probably most, methods with `::Dict` in the signature, should actually
have `::AbstractDict`, so if you have the option, it's better to relax the signature (or
submit a PR doing so) than to unwrap the `RecursiveDict`.