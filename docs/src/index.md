```@meta
CurrentModule = RecursiveDicts
DocTestSetup = quote
    using RecursiveDicts
end
```

# RecursiveDicts

A `RecursiveDict` is a wrapped `Dict`, where values may always be a `Dict` (or
`RecursiveDict`) with the same type parameters.

## Examples

It's a drop-in replacement for a standard `Dict`:

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
ERROR: MethodError: Cannot `convert` an object of type
  RecursiveDict{String, Int64} to an object of type
  Union{Int64, RecursiveDict{Symbol, Int64}}
[...]
```

## Why Does the Type Signature Look Like That?

Type parameters express the free variables in the type. In a `Dict{K,V}`, the keytype and
valtype are both free, so `K` and `V` are both the `keytype` and `valtype`, and the free parameters.

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
RecursiveDict{Int64, String} with 2 entries:
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

## Docstring

```@autodocs
Modules = [RecursiveDicts]
```
