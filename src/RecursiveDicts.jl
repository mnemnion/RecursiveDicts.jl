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
    self::Dict{K,Union{V,RecursiveDict}}
    RecursiveDict{K,V}(dict::Dict{K,Union{V,RecursiveDict}}) where {K,V} = new(dict)
    RecursiveDict(dict::AbstractDict) = new{keytype(dict), valtype(dict)}(dict)
    RecursiveDict{K,V}() where {K,V} = new(Dict{K,Union{V,RecursiveDict}}())
    function RecursiveDict{K,V}(pairs...) where {K,V}
       dict = Dict{K,Union{V,RecursiveDict}}(pairs...)
       new(dict)
    end
    function RecursiveDict(pairs::Pair...)
        dict = Dict(pairs...)
        new{keytype(dict), valtype(dict)}(dict)
    end
 end

function Base.summary(io::IO, d::RecursiveDict)
    print(io, "RecursiveDict{$(keytype(d)), $(Union{valtype(d), typeof(d)})})")
    if Base.IteratorSize(d.self) isa Base.HasLength
        n = length(d)
        print(io, " with ", n, (n==1 ? " entry" : " entries"))
    else
        print(io, "(...)")
    end
end

Base.iterate(d::RecursiveDict, i...) = iterate(d.self, i...)
Base.isiterable(d::RecursiveDict) = true
Base.in(p::Pair, d::RecursiveDict) = in(p, d.self)
Base.size(d::RecursiveDict) = size(d.self)
Base.length(d::RecursiveDict) = length(d.self)
Base.keys(d::RecursiveDict) = keys(d.self)
Base.values(d::RecursiveDict) = values(d.self)
Base.eltype(d::RecursiveDict) = eltype(d.self)
Base.keytype(d::RecursiveDict) = keytype(d.self)
Base.valtype(d::RecursiveDict) = valtype(d.self)
Base.getindex(d::RecursiveDict, k) = getindex(d.self, k)
Base.setindex!(d::RecursiveDict, v, k) = setindex!(d.self, v, k)
Base.setindex!(d::RecursiveDict, v::Dict, k) = setindex!(d.self, RecursiveDict(v), k)
Base.empty(::RecursiveDict{K,V}) where {K,V} = RecursiveDict{K,V}()
Base.empty(::RecursiveDict, ::Type{K}, ::Type{V}) where {K, V} = RecursiveDict{K,V}()
Base.convert(::Type{Dict}, d::RecursiveDict) = d.self
Base.convert(::Type{RecursiveDict}, d::Dict) = RecursiveDict(d)
Base.get(d::RecursiveDict, args...) = get(d.self, args...)
Base.get(f::Callable, d::RecursiveDict, key) = get(f, d.self, key)
Base.get!(d::RecursiveDict, args...) = get!(d.self, args...)
Base.get!(f::Callable, d::RecursiveDict, key) = get!(f, d.self, key)
Base.pop!(d::RecursiveDict, args...) = pop!(d.self, args...)

function Base.delete!(d::RecursiveDict, k::Any)
    delete!(d.self, k)
    return d
end

function Base.empty!(d::RecursiveDict)
    empty!(d.self)
    return d
end

Base.copy(d::RecursiveDict{K,V}) where {K,V} = RecursiveDict{K,V}(copy(d.self))

function Base.push!(d::RecursiveDict, p::Pair...)
    push!(d.self, p...)
    return d
end

function Base.merge(d::RecursiveDict, d2::AbstractDict)
    merge!(copy(d), d2)
end

function Base.mergewith(combine::Callable, d::RecursiveDict, others::AbstractDict...)
    mergewith!(combine, copy(d), others...)
end

end
