using RecursiveDicts
using Documenter

DocMeta.setdocmeta!(RecursiveDicts, :DocTestSetup, :(using RecursiveDicts); recursive=true)

makedocs(;
    modules=[RecursiveDicts],
    authors="Sam Atman <atmanistan@gmail.com> and contributors",
    sitename="RecursiveDicts.jl",
    format=Documenter.HTML(;
        canonical="https://mnemnion.github.io/RecursiveDicts.jl",
        edit_link="trunk",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/mnemnion/RecursiveDicts.jl",
    devbranch="trunk",
    branch="gh-pages",
    devurl="dev",
    versions=["stable" => "v^", "v#.#"],
)
