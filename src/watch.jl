using FileWatching

while true
    FileWatching.watch_file("src/es1.jl")
    include("weave.jl")
end