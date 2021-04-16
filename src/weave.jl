using Weave

OUT_DIR = joinpath(@__DIR__, "..", "weave")
SRC_DIR = joinpath(@__DIR__)

weave(joinpath(SRC_DIR, "es1.jl"), out_path=OUT_DIR)

