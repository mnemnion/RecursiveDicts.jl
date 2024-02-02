using RecursiveDicts
using Test
using Random
using Aqua

@testset "RecursiveDicts.jl" begin
    @testset "Code quality (Aqua.jl)" begin
        Aqua.test_all(RecursiveDicts)
    end
    # Copypasted from Julia core with s/Dict/RecursiveDict/g

@testset "RecursiveDict" begin
    h = RecursiveDict()
    for i=1:10000
        h[i] = i+1
    end
    for i=1:10000
        @test (h[i] == i+1)
    end
    for i=1:2:10000
        delete!(h, i)
    end
    for i=1:2:10000
        h[i] = i+1
    end
    for i=1:10000
        @test (h[i] == i+1)
    end
    for i=1:10000
        delete!(h, i)
    end
    @test isempty(h)
    h[77] = 100
    @test h[77] == 100
    for i=1:10000
        h[i] = i+1
    end
    for i=1:2:10000
        delete!(h, i)
    end
    for i=10001:20000
        h[i] = i+1
    end
    for i=2:2:10000
        @test h[i] == i+1
    end
    for i=10000:20000
        @test h[i] == i+1
    end
    h = RecursiveDict{Any,Any}("a" => 3)
    @test h["a"] == 3
    h["a","b"] = 4
    @test h["a","b"] == h[("a","b")] == 4
    h["a","b","c"] = 4
    @test h["a","b","c"] == h[("a","b","c")] == 4

    @testset "eltype, keytype and valtype" begin
        @test eltype(h) == Pair{Any,Any}
        @test keytype(h) == Any
        @test valtype(h) == Any

        td = RecursiveDict{AbstractString,Float64}()
        @test eltype(td) == Pair{AbstractString, Union{Float64, RecursiveDict{AbstractString, Float64}}}
        @test keytype(td) == AbstractString
        @test valtype(td) == Union{Float64, RecursiveDict{AbstractString, Float64}}
        @test keytype(RecursiveDict{AbstractString,Float64}) === AbstractString
        @test valtype(RecursiveDict{AbstractString,Float64}) === Union{Float64, RecursiveDict{AbstractString, Float64}}
    end
    # test rethrow of error in ctor
    @test_throws DomainError RecursiveDict((sqrt(p[1]), sqrt(p[2])) for p in zip(-1:2, -1:2))
end

let x = RecursiveDict(3=>3, 5=>5, 8=>8, 6=>6)
    pop!(x, 5)
    for k in keys(x)
        RecursiveDict{Int,Int}(x)
        @test k in [3, 8, 6]
    end
end

let z = RecursiveDict()
    get_KeyError = false
    try
        z["a"]
    catch _e123_
        get_KeyError = isa(_e123_,KeyError)
    end
    @test get_KeyError
end

_d = RecursiveDict("a"=>0)
@test isa([k for k in filter(x->length(x)==1, collect(keys(_d)))], Vector{String})

@testset "typeof" begin
    d = RecursiveDict(((1, 2), (3, 4)))
    @test d[1] === 2
    @test d[3] === 4
    d2 = RecursiveDict(1 => 2, 3 => 4)
    d3 = RecursiveDict((1 => 2, 3 => 4))
    @test d == d2 == d3
    @test typeof(d) == typeof(d2) == typeof(d3) == RecursiveDict{Int64,Int64}

    d = RecursiveDict(((1, 2), (3, "b")))
    @test d[1] === 2
    @test d[3] == "b"
    d2 = RecursiveDict(1 => 2, 3 => "b")
    d3 = RecursiveDict((1 => 2, 3 => "b"))
    @test d == d2 == d3
    @test typeof(d) == typeof(d2) == typeof(d3) == RecursiveDict{Int,Any}

    d = RecursiveDict(((1, 2), ("a", 4)))
    @test d[1] === 2
    @test d["a"] === 4
    d2 = RecursiveDict(1 => 2, "a" => 4)
    d3 = RecursiveDict((1 => 2, "a" => 4))
    @test d == d2 == d3
    @test typeof(d) == typeof(d2) == typeof(d3) == RecursiveDict{Any,Int64}

    d = RecursiveDict(((1, 2), ("a", "b")))
    @test d[1] === 2
    @test d["a"] == "b"
    d2 = RecursiveDict(1 => 2, "a" => "b")
    d3 = RecursiveDict((1 => 2, "a" => "b"))
    @test d == d2 == d3
    @test typeof(d) == typeof(d2) == typeof(d3) == RecursiveDict{Any,Any}
end

@test_throws ArgumentError first(RecursiveDict())
@test first(RecursiveDict(:f=>2)) == (:f=>2)

@testset "constructing RecursiveDicts from iterators" begin
    d = @inferred RecursiveDict(i=>i for i=1:3)
    @test isa(d, RecursiveDict{Int64,Int64})
    @test d == RecursiveDict(1=>1, 2=>2, 3=>3)
    d = RecursiveDict(i==1 ? (1=>2) : (2.0=>3.0) for i=1:2)
    @test isa(d, RecursiveDict{Real,Real})
    @test d == RecursiveDict{Real,Real}(2.0=>3.0, 1=>2)

    # issue #39117
    @test RecursiveDict(t[1]=>t[2] for t in zip((1,"2"), (2,"2"))) == RecursiveDict{Any,Any}(1=>2, "2"=>"2")
end

@testset "empty tuple ctor" begin
    h = RecursiveDict(())
    @test length(h) == 0
end

@testset "type of RecursiveDict constructed from varargs of Pairs" begin
    @test RecursiveDict(1=>1, 2=>2.0) isa RecursiveDict{Int,Real}
    @test RecursiveDict(1=>1, 2.0=>2) isa RecursiveDict{Real,Int}
    @test RecursiveDict(1=>1.0, 2.0=>2) isa RecursiveDict{Real,Real}

    for T in (Nothing, Missing)
        @test RecursiveDict(1=>1, 2=>T()) isa RecursiveDict{Int,Union{Int,T}}
        @test RecursiveDict(1=>T(), 2=>2) isa RecursiveDict{Int,Union{Int,T}}
        @test RecursiveDict(1=>1, T()=>2) isa RecursiveDict{Union{Int,T},Int}
        @test RecursiveDict(T()=>1, 2=>2) isa RecursiveDict{Union{Int,T},Int}
    end
end

@test_throws KeyError RecursiveDict("a"=>2)[Base.secret_table_token]

@testset "issue #1821" begin
    d = RecursiveDict{String, Vector{Int}}()
    d["a"] = [1, 2]
    @test_throws MethodError d["b"] = 1
    @test isa(repr(d), AbstractString)  # check that printable without error
end

@testset "issue #2344" begin
    local bar
    bestkey(d, key) = key
    bestkey(d::RecursiveDict{K,V}, key) where {K<:AbstractString,V} = string(key)
    bar(x) = bestkey(x, :y)
    @test bar(RecursiveDict(:x => [1,2,5])) === :y
    @test bar(RecursiveDict("x" => [1,2,5])) == "y"
end

mutable struct I1438T
    id
end
Base.hash(x::I1438T, h::UInt) = hash(x.id, h)

@testset "issue #1438" begin
    seq = [26, 28, 29, 30, 31, 32, 33, 34, 35, 36, -32, -35, -34, -28, 37, 38, 39, 40, -30,
           -31, 41, 42, 43, 44, -33, -36, 45, 46, 47, 48, -37, -38, 49, 50, 51, 52, -46, -50, 53]
    xs = [ I1438T(id) for id = 1:53 ]
    s = Set()
    for id in seq
        if id > 0
            x = xs[id]
            push!(s, x)
            @test in(x, s)                 # check that x can be found
        else
            delete!(s, xs[-id])
        end
    end
end

@testset "equality" for eq in (isequal, ==)
    @test  eq(RecursiveDict(), RecursiveDict())
    @test  eq(RecursiveDict(1 => 1), RecursiveDict(1 => 1))
    @test !eq(RecursiveDict(1 => 1), RecursiveDict())
    @test !eq(RecursiveDict(1 => 1), RecursiveDict(1 => 2))
    @test !eq(RecursiveDict(1 => 1), RecursiveDict(2 => 1))

    # Generate some data to populate dicts to be compared
    data_in = [ (rand(1:1000), randstring(2)) for _ in 1:1001 ]

    # Populate the first dict
    d1 = RecursiveDict{Int, AbstractString}()
    for (k, v) in data_in
        d1[k] = v
    end
    data_in = collect(d1)
    # shuffle the data
    for i in 1:length(data_in)
        j = rand(1:length(data_in))
        data_in[i], data_in[j] = data_in[j], data_in[i]
    end
    # Inserting data in different (shuffled) order should result in
    # equivalent dict.
    d2 = RecursiveDict{Int, AbstractString}()
    for (k, v) in data_in
        d2[k] = v
    end

    @test eq(d1, d2)
    d3 = copy(d2)
    d4 = copy(d2)
    # Removing an item gives different dict
    delete!(d1, data_in[rand(1:length(data_in))][1])
    @test !eq(d1, d2)
    # Changing a value gives different dict
    d3[data_in[rand(1:length(data_in))][1]] = randstring(3)
    !eq(d1, d3)
    # Adding a pair gives different dict
    d4[1001] = randstring(3)
    @test !eq(d1, d4)

    @test eq(RecursiveDict(), sizehint!(RecursiveDict(),96))

    # RecursiveDictionaries of different types
    @test !eq(RecursiveDict(1 => 2), RecursiveDict("dog" => "bone"))
    @test eq(RecursiveDict{Int,Int}(), RecursiveDict{AbstractString,AbstractString}())
end

@testset "equality special cases" begin
    @test RecursiveDict(1=>0.0) == RecursiveDict(1=>-0.0)
    @test !isequal(RecursiveDict(1=>0.0), RecursiveDict(1=>-0.0))

    @test RecursiveDict(0.0=>1) != RecursiveDict(-0.0=>1)
    @test !isequal(RecursiveDict(0.0=>1), RecursiveDict(-0.0=>1))

    @test RecursiveDict(1=>NaN) != RecursiveDict(1=>NaN)
    @test isequal(RecursiveDict(1=>NaN), RecursiveDict(1=>NaN))

    @test RecursiveDict(NaN=>1) == RecursiveDict(NaN=>1)
    @test isequal(RecursiveDict(NaN=>1), RecursiveDict(NaN=>1))

    @test ismissing(RecursiveDict(1=>missing) == RecursiveDict(1=>missing))
    @test isequal(RecursiveDict(1=>missing), RecursiveDict(1=>missing))
    d = RecursiveDict(1=>missing)
    @test ismissing(d == d)
    d = RecursiveDict(1=>[missing])
    @test ismissing(d == d)
    d = RecursiveDict(1=>NaN)
    @test d != d
    @test isequal(d, d)

    @test RecursiveDict(missing=>1) == RecursiveDict(missing=>1)
    @test isequal(RecursiveDict(missing=>1), RecursiveDict(missing=>1))
end

@testset "get!" begin # (get with default values assigned to the given location)
    f(x) = x^2
    d = RecursiveDict(8=>19)
    @test get!(d, 8, 5) == 19
    @test get!(d, 19, 2) == 2

    @test get!(d, 42) do  # d is updated with f(2)
        f(2)
    end == 4

    @test get!(d, 42) do  # d is not updated
        f(200)
    end == 4

    @test get(d, 13) do   # d is not updated
        f(4)
    end == 16

    @test d == RecursiveDict(8=>19, 19=>2, 42=>4)
end

@testset "getkey" begin
   h = RecursiveDict(1=>2, 3 => 6, 5=>10)
   @test getkey(h, 1, 7) == 1
   @test getkey(h, 4, 6) == 6
   @test getkey(h, "1", 8) == 8
end

@testset "show" begin
    for d in (RecursiveDict("\n" => "\n", "1" => "\n", "\n" => "2"),
              RecursiveDict(string(i) => i for i = 1:30),
              RecursiveDict(reshape(1:i^2,i,i) => reshape(1:i^2,i,i) for i = 1:24),
              RecursiveDict(String(Char['α':'α'+i;]) => String(Char['α':'α'+i;]) for i = (1:10)*10),
              RecursiveDict("key" => zeros(0, 0)))
        for cols in (12, 40, 80), rows in (2, 10, 24)
            # Ensure output is limited as requested
            s = IOBuffer()
            io = Base.IOContext(s, :limit => true, :displaysize => (rows, cols))
            Base.show(io, MIME("text/plain"), d)
            out = split(String(take!(s)),'\n')
            for line in out[2:end]
                @test textwidth(line) <= cols
            end
            @test length(out) <= rows

            for f in (keys, values)
                s = IOBuffer()
                io = Base.IOContext(s, :limit => true, :displaysize => (rows, cols))
                Base.show(io, MIME("text/plain"), f(d))
                out = split(String(take!(s)),'\n')
                for line in out[2:end]
                    @test textwidth(line) <= cols
                end
                @test length(out) <= rows
            end
        end
        # Simply ensure these do not throw errors
        Base.show(IOBuffer(), d)
        @test !isempty(summary(d))
        @test !isempty(summary(keys(d)))
        @test !isempty(summary(values(d)))
    end
    # show on empty RecursiveDict
    io = IOBuffer()
    d = RecursiveDict{Int, String}()
    show(io, d)
    str = String(take!(io))
    @test str == "RecursiveDict{$(Int), String}()"
    close(io)
end

struct RainbowString
    s::String
    bold::Bool
    other::Bool
    valid::Bool
    offset::Int
end
RainbowString(s, bold=false, other=false, valid=true) = RainbowString(s, bold, other, valid, 0)

function Base.show(io::IO, rbs::RainbowString)
    for (i, s) in enumerate(rbs.s)
        if i ≤ rbs.offset
            print(io, s)
            continue
        end
        color = rbs.other ? string("\033[4", rand(1:7), 'm') : Base.text_colors[rand(0:255)]
        if rbs.bold
            printstyled(io, color, s; bold=true)
        else
            print(io, color, s)
        end
        if rbs.valid
            print(io, '\033', '[', rbs.other ? "0" : "39", 'm')  # end of color marker
        end
    end
end

@testset "Display with colors" begin
    d = RecursiveDict([randstring(8) => [RainbowString(randstring(8)) for i in 1:10] for j in 1:5]...)
    str = sprint(io -> show(io, MIME("text/plain"), d); context = (:displaysize=>(30,80), :color=>true, :limit=>true))
    lines = split(str, '\n')
    @test all(endswith("\033[0m…"), lines[2:end])
    @test all(x -> length(x) > 100, lines[2:end])

    d2 = RecursiveDict(:foo => RainbowString("bar"))
    str2 = sprint(io -> show(io, MIME("text/plain"), d2); context = (:displaysize=>(30,80), :color=>true, :limit=>true))
    @test !occursin('…', str2)
    @test endswith(str2, "\033[0m")

    d3 = RecursiveDict(:foo => RainbowString("bar", true))
    str3 = sprint(io -> show(io, MIME("text/plain"), d3); context = (:displaysize=>(30,80), :color=>true, :limit=>true))
    @test !occursin('…', str3)
    @test endswith(str3, "\033[0m")

    d4 = RecursiveDict(RainbowString(randstring(8), true) => nothing)
    str4 = sprint(io -> show(io, MIME("text/plain"), d4); context = (:displaysize=>(30,20), :color=>true, :limit=>true))
    @test endswith(str4, "\033[0m… => nothing")

    d5 = RecursiveDict(RainbowString(randstring(30), false, true, false) => nothing)
    str5 = sprint(io -> show(io, MIME("text/plain"), d5); context = (:displaysize=>(30,30), :color=>true, :limit=>true))
    @test endswith(str5, "\033[0m… => nothing")

    d6 = RecursiveDict(randstring(8) => RainbowString(randstring(30), true, true, false) for _ in 1:3)
    str6 = sprint(io -> show(io, MIME("text/plain"), d6); context = (:displaysize=>(30,30), :color=>true, :limit=>true))
    lines6 = split(str6, '\n')
    @test all(endswith("\033[0m…"), lines6[2:end])
    @test all(x -> length(x) > 100, lines6[2:end])
    str6_long = sprint(io -> show(io, MIME("text/plain"), d6); context = (:displaysize=>(30,80), :color=>true, :limit=>true))
    lines6_long = split(str6_long, '\n')
    @test all(endswith("\033[0m"), lines6_long[2:end])

    d7 = RecursiveDict(randstring(8) => RainbowString(randstring(30)))
    str7 = sprint(io -> show(io, MIME("text/plain"), d7); context = (:displaysize=>(30,20), :color=>true, :limit=>true))
    line7 = split(str7, '\n')[2]
    @test endswith(line7, "\033[0m…")
    @test length(line7) > 100

    d8 = RecursiveDict(:x => RainbowString(randstring(10), false, false, false, 6))
    str8 = sprint(io -> show(io, MIME("text/plain"), d8); context = (:displaysize=>(30,14), :color=>true, :limit=>true))
    line8 = split(str8, '\n')[2]
    @test !occursin("\033[", line8)
    @test length(line8) == 14
    str8_long = sprint(io -> show(io, MIME("text/plain"), d8); context = (:displaysize=>(30,16), :color=>true, :limit=>true))
    line8_long = split(str8_long, '\n')[2]
    @test endswith(line8_long, "\033[0m…")
    @test length(line8_long) > 20

    d9 = RecursiveDict(:x => RainbowString(repeat('苹', 5), false, true, false))
    str9 = sprint(io -> show(io, MIME("text/plain"), d9); context = (:displaysize=>(30,15), :color=>true, :limit=>true))
    @test endswith(str9, "\033[0m…")
    @test count('苹', str9) == 3

    d10 = RecursiveDict(:xy => RainbowString(repeat('苹', 5), false, true, false))
    str10 = sprint(io -> show(io, MIME("text/plain"), d10); context = (:displaysize=>(30,15), :color=>true, :limit=>true))
    @test endswith(str10, "\033[0m…")
    @test count('苹', str10) == 2

    d11 = RecursiveDict(RainbowString("abcdefgh", false, true, false) => 0, "123456" => 1)
    str11 = sprint(io -> show(io, MIME("text/plain"), d11); context = (:displaysize=>(30,80), :color=>true, :limit=>true))
    _, line11_a, line11_b = split(str11, '\n')
    @test endswith(line11_a, "h\033[0m => 0") || endswith(line11_b, "h\033[0m => 0")
    @test endswith(line11_a, "6\" => 1") || endswith(line11_b, "6\" => 1")

    d12 = RecursiveDict(RainbowString(repeat(Char(48+i), 4), (i&1)==1, (i&2)==2, (i&4)==4) => i for i in 1:8)
    str12 = sprint(io -> show(io, MIME("text/plain"), d12); context = (:displaysize=>(30,80), :color=>true, :limit=>true))
    @test !occursin('…', str12)

    d13 = RecursiveDict(RainbowString("foo\nbar") => 74)
    str13 = sprint(io -> show(io, MIME("text/plain"), d13); context = (:displaysize=>(30,80), :color=>true, :limit=>true))
    @test count('\n', str13) == 1
    @test occursin('…', str13)
end

@testset "Issue #15739" begin # Compact REPL printouts of an `AbstractRecursiveDict` use brackets when appropriate
    d = RecursiveDict((1=>2) => (3=>45), (3=>10) => (10=>11))
    buf = IOBuffer()
    show(IOContext(buf, :compact => true), d)

    # Check explicitly for the expected strings, since the CPU bitness effects
    # dictionary ordering.
    result = String(take!(buf))
    @test occursin("RecursiveDict", result)
    @test occursin("(1=>2)=>(3=>45)", result)
    @test occursin("(3=>10)=>(10=>11)", result)
end

mutable struct Alpha end
Base.show(io::IO, ::Alpha) = print(io,"α")
@testset "issue #9463" begin
    sbuff = IOBuffer()
    io = Base.IOContext(sbuff, :limit => true, :displaysize => (10, 20))

    Base.show(io, MIME("text/plain"), RecursiveDict(Alpha()=>1))
    local str = String(take!(sbuff))
    @test !occursin("…", str)
    @test endswith(str, "α => 1")
end

@testset "issue #2540" begin
    d = RecursiveDict{Any,Any}(RecursiveDict(x => 1 for x in ['a', 'b', 'c']))
    @test d == RecursiveDict('a'=>1, 'b'=>1, 'c'=> 1)
end

@testset "issue #2629" begin
    d = RecursiveDict{AbstractString,AbstractString}(RecursiveDict(a=>"foo" for a in ["a","b","c"]))
    @test d == RecursiveDict("a"=>"foo","b"=>"foo","c"=>"foo")
end

@testset "issue #5886" begin
    d5886 = RecursiveDict()
    for k5886 in 1:11
       d5886[k5886] = 1
    end
    for k5886 in keys(d5886)
       # undefined ref if not fixed
       d5886[k5886] += 1
    end
end

@testset "issue 9295" begin
    d = RecursiveDict()
    @test push!(d, 'a' => 1) === d
    @test d['a'] == 1
    @test push!(d, 'b' => 2, 'c' => 3) === d
    @test d['b'] == 2
    @test d['c'] == 3
    @test push!(d, 'd' => 4, 'e' => 5, 'f' => 6) === d
    @test d['d'] == 4
    @test d['e'] == 5
    @test d['f'] == 6
    @test length(d) == 6
end


@testset "Issue #7944" begin
    d = RecursiveDict{Int,Int}()
    get!(d, 0) do
        d[0] = 1
    end
    @test length(d) == 1
end

@testset "iteration" begin
    d = RecursiveDict('a'=>1, 'b'=>1, 'c'=> 3)
    @test [d[k] for k in keys(d)] == [d[k] for k in eachindex(d)] ==
          [v for (k, v) in d] == [d[x[1]] for (i, x) in enumerate(d)]
end

@testset "generators, similar" begin
    d = RecursiveDict(:a=>"a")
    # TODO: restore when 0.7 deprecation is removed
    #@test @inferred(map(identity, d)) == d
end

@testset "Issue 12451" begin
    @test_throws ArgumentError RecursiveDict(0)
    @test_throws ArgumentError RecursiveDict([1])
    @test_throws ArgumentError RecursiveDict([(1,2),0])
end

@test_throws InexactError RecursiveDict(convert(Int,1.5) for i=1:1)

@testset "filtering" begin
    d = RecursiveDict(zip(1:1000,1:1000))
    f = p -> iseven(p.first)
    @test filter(f, d) == filter!(f, copy(d)) ==
          invoke(filter!, Tuple{Function,RecursiveDict}, f, copy(d)) ==
          RecursiveDict(zip(2:2:1000, 2:2:1000))
    d = RecursiveDict(zip(-1:3,-1:3))
    f = p -> sqrt(p.second) > 2
    # test rethrowing error from f
    @test_throws DomainError filter(f, d)
end

struct MyString <: AbstractString
    str::String
end
struct MyInt <: Integer
    val::UInt
end

const global hashoffset = [UInt(190)]

Base.hash(s::MyString) = hash(s.str) + hashoffset[]
Base.lastindex(s::MyString) = lastindex(s.str)
Base.iterate(s::MyString, v::Int=1) = iterate(s.str, v)
Base.isequal(a::MyString, b::MyString) = isequal(a.str, b.str)
Base.:(==)(a::MyString, b::MyString) = (a.str == b.str)

Base.hash(v::MyInt) = v.val + hashoffset[]
Base.lastindex(v::MyInt) = lastindex(v.val)
Base.iterate(v::MyInt, i...) = iterate(v.val, i...)
Base.isequal(a::MyInt, b::MyInt) = isequal(a.val, b.val)
Base.:(==)(a::MyInt, b::MyInt) = (a.val == b.val)
@testset "issue #15077" begin
    let badKeys = [
        "FINO_emv5.0","FINO_ema0.1","RATE_ema1.0","NIBPM_ema1.0",
        "SAO2_emv5.0","O2FLOW_ema5.0","preop_Neuro/Psych_","gender_",
        "FIO2_ema0.1","PEAK_ema5.0","preop_Reproductive_denies","O2FLOW_ema0.1",
        "preop_Endocrine_denies","preop_Respiratory_",
        "NIBPM_ema0.1","PROPOFOL_MCG/KG/MIN_decay5.0","NIBPD_ema1.0","NIBPS_ema5.0",
        "anesthesiaStartTime","NIBPS_ema1.0","RESPRATE_ema1.0","PEAK_ema0.1",
        "preop_GU_denies","preop_Cardiovascular_","PIP_ema5.0","preop_ENT_denies",
        "preop_Skin_denies","preop_Renal_denies","asaCode_IIIE","N2OFLOW_emv5.0",
        "NIBPD_emv5.0", # <--- here is the key that we later can't find
        "NIBPM_ema5.0","preop_Respiratory_complete","ETCO2_ema5.0",
        "RESPRATE_ema0.1","preop_Functional Status_<2","preop_Renal_symptoms",
        "ECGRATE_ema5.0","FIO2_emv5.0","RESPRATE_emv5.0","7wu3ty0a4fs","BVO",
        "4UrCWXUsaT"
    ]
        local d = RecursiveDict{AbstractString,Int}()
        for i = 1:length(badKeys)
            d[badKeys[i]] = i
        end
        # Check all keys for missing values
        for i = 1:length(badKeys)
            @test d[badKeys[i]] == i
        end

        # Walk through all possible hash values (mod size of hash table)
        for offset = 0:1023
            local d2 = RecursiveDict{MyString,Int}()
            hashoffset[] = offset
            for i = 1:length(badKeys)
                d2[MyString(badKeys[i])] = i
            end
            # Check all keys for missing values
            for i = 1:length(badKeys)
                @test d2[MyString(badKeys[i])] == i
            end
        end
    end


    let badKeys = UInt16[0xb800,0xa501,0xcdff,0x6303,0xe40a,0xcf0e,0xf3df,0xae99,0x9913,0x741c,
                         0xd01f,0xc822,0x9723,0xb7a0,0xea25,0x7423,0x6029,0x202a,0x822b,0x492c,
                         0xd02c,0x862d,0x8f34,0xe529,0xf938,0x4f39,0xd03a,0x473b,0x1e3b,0x1d3a,
                         0xcc39,0x7339,0xcf40,0x8740,0x813d,0xe640,0xc443,0x6344,0x3744,0x2c3d,
                         0x8c48,0xdf49,0x5743]
        # Walk through all possible hash values (mod size of hash table)
        for offset = 0:1023
            local d2 = RecursiveDict{MyInt, Int}()
            hashoffset[] = offset
            for i = 1:length(badKeys)
                d2[MyInt(badKeys[i])] = i
            end
            # Check all keys for missing values
            for i = 1:length(badKeys)
                @test d2[MyInt(badKeys[i])] == i
            end
        end
    end
end

# #18213
RecursiveDict(1 => rand(2,3), 'c' => "asdf") # just make sure this does not trigger a deprecation

@testset "issue #19995, hash of dicts" begin
    @test hash(RecursiveDict(RecursiveDict(1=>2) => 3, RecursiveDict(4=>5) => 6)) != hash(RecursiveDict(RecursiveDict(4=>5) => 3, RecursiveDict(1=>2) => 6))
    a = RecursiveDict(RecursiveDict(3 => 4, 2 => 3) => 2, RecursiveDict(1 => 2, 5 => 6) => 1)
    b = RecursiveDict(RecursiveDict(1 => 2, 2 => 3, 5 => 6) => 1, RecursiveDict(3 => 4) => 2)
    @test hash(a) != hash(b)
end


@testset "issue #18708 error type for dict constructor" begin
    @test_throws UndefVarError RecursiveDict(x => y for x in 1:10)
end

mutable struct Error19179 <: Exception
end

@testset "issue #19179 throwing error in dict constructor" begin
    @test_throws Error19179 RecursiveDict(i => throw(Error19179()) for i in 1:10)
end

# issue #18090
let
    d = RecursiveDict(i => i^2 for i in 1:10_000)
    z = zip(keys(d), values(d))
    for (pair, tupl) in zip(d, z)
        @test pair[1] == tupl[1] && pair[2] == tupl[2]
    end
end

struct NonFunctionCallable end
(::NonFunctionCallable)(args...) = +(args...)

@testset "RecursiveDict merge" begin
    d1 = RecursiveDict("A" => 1, "B" => 2)
    d2 = RecursiveDict("B" => 3, "C" => 4)
    @test @inferred merge(d1, d2) == RecursiveDict("A" => 1, "B" => 3, "C" => 4)
    # merge with combiner function
    @test @inferred mergewith(+, d1, d2) == RecursiveDict("A" => 1, "B" => 5, "C" => 4)
    @test @inferred mergewith(*, d1, d2) == RecursiveDict("A" => 1, "B" => 6, "C" => 4)
    @test @inferred mergewith(-, d1, d2) == RecursiveDict("A" => 1, "B" => -1, "C" => 4)
    @test @inferred mergewith(NonFunctionCallable(), d1, d2) == RecursiveDict("A" => 1, "B" => 5, "C" => 4)
    @test foldl(mergewith(+), [d1, d2]; init=RecursiveDict{keytype(d1),valtype(d1)}()) ==
        RecursiveDict("A" => 1, "B" => 5, "C" => 4)
    # backward compatibility
    @test @inferred merge(+, d1, d2) == RecursiveDict("A" => 1, "B" => 5, "C" => 4)
end

@testset "RecursiveDict merge!" begin
    d1 = RecursiveDict("A" => 1, "B" => 2)
    d2 = RecursiveDict("B" => 3, "C" => 4)
    @inferred merge!(d1, d2)
    @test d1 == RecursiveDict("A" => 1, "B" => 3, "C" => 4)
    # merge! with combiner function
    @inferred mergewith!(+, d1, d2)
    @test d1 == RecursiveDict("A" => 1, "B" => 6, "C" => 8)
    @inferred mergewith!(*, d1, d2)
    @test d1 == RecursiveDict("A" => 1, "B" => 18, "C" => 32)
    @inferred mergewith!(-, d1, d2)
    @test d1 == RecursiveDict("A" => 1, "B" => 15, "C" => 28)
    @inferred mergewith!(NonFunctionCallable(), d1, d2)
    @test d1 == RecursiveDict("A" => 1, "B" => 18, "C" => 32)
    @test foldl(mergewith!(+), [d1, d2]; init=empty(d1)) ==
        RecursiveDict("A" => 1, "B" => 21, "C" => 36)
    # backward compatibility
    merge!(+, d1, d2)
    @test d1 == RecursiveDict("A" => 1, "B" => 21, "C" => 36)
end

@testset "RecursiveDict reduce merge" begin
    function check_merge(i::Vector{<:RecursiveDict}, o)
        r1 = reduce(merge, i)
        r2 = merge(i...)
        t = typeof(o)
        @test r1 == o
        @test r2 == o
        @test typeof(r1) == t
        @test typeof(r2) == t
    end
    check_merge([RecursiveDict(1=>Complex(2.0, 0.0)), RecursiveDict(2=>Complex(1.0, 1.0))],
      RecursiveDict(2=>Complex(1.0, 1.0), 1=>Complex(2.0, 0.0)))
    check_merge([RecursiveDict(1=>2), RecursiveDict(3=>4)], RecursiveDict(3=>4, 1=>2))
end


@testset "misc error/io" begin
    d = RecursiveDict('a'=>1, 'b'=>1, 'c'=> 3)
    @test_throws ErrorException 'a' in d
    key_str = sprint(show, keys(d))
    @test 'a' ∈ key_str
    @test 'b' ∈ key_str
    @test 'c' ∈ key_str
end

@testset "RecursiveDict pop!" begin
    d = RecursiveDict(1=>2, 3=>4)
    @test pop!(d, 1) == 2
    @test_throws KeyError pop!(d, 1)
    @test pop!(d, 1, 0) == 0
    @test pop!(d) == (3=>4)
    @test_throws ArgumentError pop!(d)
end

@testset "keys as a set" begin
    d = RecursiveDict(1=>2, 3=>4)
    @test keys(d) isa AbstractSet
    @test empty(keys(d)) isa AbstractSet
    let i = keys(d) ∩ Set([1,2])
        @test i isa AbstractSet
        @test i == Set([1])
    end
    @test Set(string(k) for k in keys(d)) == Set(["1","3"])
end

@testset "find" begin
    @test findall(isequal(1), RecursiveDict(:a=>1, :b=>2)) == [:a]
    @test sort(findall(isequal(1), RecursiveDict(:a=>1, :b=>1))) == [:a, :b]
    @test isempty(findall(isequal(1), RecursiveDict()))
    @test isempty(findall(isequal(1), RecursiveDict(:a=>2, :b=>3)))

    @test findfirst(isequal(1), RecursiveDict(:a=>1, :b=>2)) === :a
    @test findfirst(isequal(1), RecursiveDict(:a=>1, :b=>1, :c=>3)) in (:a, :b)
    @test findfirst(isequal(1), RecursiveDict()) === nothing
    @test findfirst(isequal(1), RecursiveDict(:a=>2, :b=>3)) === nothing
end


@testset "copy!" begin
    s = RecursiveDict(1=>2, 2=>3)
    for a = ([3=>4], [3=>4], [3=>4, 5=>6, 7=>8], Pair{Int,Int}[3=>4, 5=>6, 7=>8])
        @test s === copy!(s, RecursiveDict(a)) == RecursiveDict(a)
    end
    s2 = copy(s)
    @test copy!(s, s) == s2
end

# getindex is :effect_free and :terminates but not :consistent
for T in (Int, Float64, String, Symbol)
    @testset "Compiler Intrinsics" begin
        let T=T
            @test !Core.Compiler.is_consistent(Base.infer_effects(getindex, (RecursiveDict{T,Any}, T)))
            @test_skip Core.Compiler.is_effect_free(Base.infer_effects(getindex, (RecursiveDict{T,Any}, T)))
            @test !Core.Compiler.is_nothrow(Base.infer_effects(getindex, (RecursiveDict{T,Any}, T)))
            @test Core.Compiler.is_terminates(Base.infer_effects(getindex, (RecursiveDict{T,Any}, T)))
        end
    end
end

struct BadHash
    i::Int
end
Base.hash(::BadHash, ::UInt)=UInt(1)
@testset "maxprobe reset #51595" begin
    d = RecursiveDict(BadHash(i)=>nothing for i in 1:20)
    empty!(d)
    sizehint!(d, 0)
    @test_broken d.self.maxprobe < length(keys(d))
    d[BadHash(1)]=nothing
    @test !(BadHash(2) in keys(d))
    d = RecursiveDict(BadHash(i)=>nothing for i in 1:20)
    for _ in 1:20
        pop!(d)
    end
    sizehint!(d, 0)
    @test_broken d.self.maxprobe < length(keys(d))
    d[BadHash(1)]=nothing
    @test !(BadHash(2) in keys(d))
end

# Issue #52066
let d = RecursiveDict()
    d[1] = 'a'
    d[1.0] = 'b'
    @test only(d) === Pair{Any,Any}(1.0, 'b')
end

@testset "UnionAll `keytype` and `valtype` (issue #53115)" begin
    K = Int8
    V = Int16
    @test_throws MethodError keytype(RecursiveDict)
    @test_throws MethodError keytype(RecursiveDict{<:Any,V})
    @test_broken             keytype(RecursiveDict{K      }) == K
    @test                    keytype(RecursiveDict{K,    V}) == K

    @test_throws MethodError valtype(RecursiveDict)
    @test                    valtype(RecursiveDict{K,V}) == Union{V,RecursiveDict{K,V}}
    @test_throws MethodError valtype(RecursiveDict{K      })
    @test                    valtype(RecursiveDict{K,    V}) == Union{V,RecursiveDict{K,V}}
end

end
