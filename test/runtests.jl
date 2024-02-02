using RecursiveDicts
using Test
using Aqua

@testset "RecursiveDicts.jl" begin
    @testset "Code quality (Aqua.jl)" begin
        Aqua.test_all(RecursiveDicts)
    end
    # Write your tests here.
end
