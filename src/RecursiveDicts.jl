module RecursiveDicts

import Base: Callable

export RecursiveDict


"""
    RecursiveDict{K,V} <: AbstractDict{K,Union{V,RecursiveDict}}

A `Dict` where an instance of itself is always a valid value.  So a
`RecursiveDict{String,String}` has `String` keys, and the values may be either
`String`s or a `RecursiveDict{String,String}`. It's harmless to write this as
`RecursiveDict{String,Union{String,RecursiveDict{String,String}}}`, although it
isn't necessary, and as the signature gestures at, one does have to provide the
base case eventually.

Implements all `Base` methods which take a `Dict`.  `convert` will return the wrapped
`Dict` which provides the data structure, as a shared reference, meaning changes to
the provided `Dict` will be seen in the `RecursiveDict`, so this data structure may
be used anywhere which expects a `Dict`, with a bit of care.

A `Dict{K,V}` may also be converted to a `RecursiveDict{K,V}`, without copying.
"""
struct RecursiveDict{K,V} <: AbstractDict{K,Union{V,RecursiveDict}}
    self::Dict{K,Union{V,RecursiveDict{K,V}}}

    RecursiveDict{K,V}(dict::Dict{K,Union{V,RecursiveDict{K,V}}}) where {K,V} = new(dict)
    RecursiveDict{K,V}(args...) where {K,V} = new(Dict{K,Union{V,RecursiveDict{K,V}}}(args...))
    RecursiveDict{K,V}() where {K,V} = new(Dict{K,Union{V,RecursiveDict{K,V}}}())
    function RecursiveDict{K,V}(pairs::Pair...) where {K,V}
        dict = Dict{K,Union{V,RecursiveDict{K,V}}}(pairs...)
        new(dict)
    end
    function RecursiveDict{K,V}(dict::AbstractDict{K,V}) where {K,V}
        d = Dict{K, Union{V,RecursiveDict{K,V}}}(collect(pairs(dict))...)
        new(d)
    end
    function RecursiveDict(args...)
        dict = Dict(args...)
        new{keytype(dict),valtype(dict)}(dict)
    end
end

@inline
Base.iterate(d::RecursiveDict, i...) = iterate(d.self, i...)

@inline
Base.isiterable(d::RecursiveDict) = true

@inline
Base.in(p::Pair, d::RecursiveDict) = in(p, d.self)

@inline
Base.size(d::RecursiveDict) = size(d.self)

@inline
Base.length(d::RecursiveDict) = length(d.self)

@inline
Base.keys(d::RecursiveDict) = keys(d.self)

@inline
Base.values(d::RecursiveDict) = values(d.self)

@inline
Base.eltype(d::RecursiveDict) = eltype(d.self)

@inline
Base.keytype(d::RecursiveDict) = keytype(d.self)

@inline
Base.valtype(::Type{RecursiveDict{K,V}}) where {K,V} = Union{V,RecursiveDict{K,V}}

@inline
Base.getindex(d::RecursiveDict, k) = getindex(d.self, k)

@inline
Base.setindex!(d::RecursiveDict, v, k...) = setindex!(d.self, v, k...)

@inline
Base.setindex!(d::RecursiveDict, v::AbstractDict, k...) = setindex!(d.self, RecursiveDict(v), k...)

@inline
Base.empty(::RecursiveDict{K,V}) where {K,V} = RecursiveDict{K,V}()

@inline
Base.empty(::RecursiveDict, ::Type{K}, ::Type{V}) where {K, V} = RecursiveDict{K,V}()

@inline
Base.convert(::Type{Dict}, d::RecursiveDict) = d.self

@inline
Base.convert(::Type{RecursiveDict}, d::Dict) = RecursiveDict(d)

@inline
Base.get(d::RecursiveDict, args...) = get(d.self, args...)

@inline
Base.get(f::Callable, d::RecursiveDict, key) = get(f, d.self, key)

@inline
Base.getkey(d::RecursiveDict, args...) = getkey(d.self, args...)

@inline
Base.get!(d::RecursiveDict, args...) = get!(d.self, args...)

@inline
Base.get!(f::Callable, d::RecursiveDict, key) = get!(f, d.self, key)

@inline
Base.pop!(d::RecursiveDict, args...) = pop!(d.self, args...)

@inline
Base.sizehint!(d::RecursiveDict, args...; kwargs...) = sizehint!(d.self, args...; kwargs...)

@inline
function Base.delete!(d::RecursiveDict, k::Any)
    delete!(d.self, k)
    return d
end

@inline
function Base.empty!(d::RecursiveDict)
    empty!(d.self)
    return d
end

@inline
Base.copy(d::RecursiveDict{K,V}) where {K,V} = RecursiveDict{K,V}(copy(d.self))

@inline
function Base.push!(d::RecursiveDict, p::Pair...)
    push!(d.self, p...)
    return d
end

@inline
function Base.merge(d::RecursiveDict, d2::AbstractDict)
    merge!(copy(d), RecursiveDict(d2))
end

@inline
function Base.mergewith(combine::Callable, d::RecursiveDict, others::AbstractDict...)
    mergewith!(combine, copy(d), others...)
end

end
