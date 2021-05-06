#' # Genetic Algorithm for Solving the N-Queens problem

abstract type Genome end

struct NQueensGenome <: Genome 
    board::Vector{Int}
    fit::Float64 # fitness
end

using Random

function randgenome(n)
    board = Vector{Int}(undef, n)
    rand!(board, 1:n)
    NQueensGenome(board, fitness(board))
end

function fitness(board)
    attacks = 0 # number of pairs that attack each other
    n = length(board)
    for i in 1:n
        for j in i+1:n
            attacks += Int(board[i] == board[j] || abs(i-j) == abs(board[i]-board[j]))
        end
    end

    return 1.0/(attacks+1)
end

function fitness(g::NQueensGenome) g.fit end

function cmp_genome(a::NQueensGenome, b::NQueensGenome) a.board == b.board end

function crossover(a::NQueensGenome, b::NQueensGenome)
    n = length(a.board)
    @assert n == length(b.board)

    seed = rand(1:n)

    a_part = a.board[1:seed-1]
    b_part = b.board[seed:n]

    board = vcat(a_part, b_part)
    @assert length(board) == n
    NQueensGenome(board, fitness(board))
end

function mutate!(g::NQueensGenome, mutation_rate::Float64)
    @assert 0 <= mutation_rate <= 1
    n = length(g.board)
    for i in 1:n
        if rand() <= mutation_rate
            g.board[i] = rand(1:n)
        end
    end
end

function evolve(population::Vector{S}; mutation_rate=0.05) where {S<:Genome}
    pop_size = length(population)
    # min_mating_pool = Int(floor(pop_size/2)) # 10%
    # max_mating_pool = pop_size*2 #Int(floor((pop_size/100) * 90)) # 90%
    # mating_pool_size = rand(min_mating_pool:max_mating_pool)
    mating_pool_size = pop_size
    # @show mating_pool_size
    mating_pool = Vector{Pair{S, S}}(undef, mating_pool_size)
    for i in 1:mating_pool_size
        # choose parent A with a chance based on its fitness
        # higher fitness has a higher chance to reproduce
        parent_a = rand(population)
        while true
            if rand() <= fitness(parent_a)
                break
            end
            parent_a = rand(population)
        end  

        # choose parent B with a different genome and a chance based on its fitness
        parent_b = rand(population)
        while true 
            j = 0
            # try to avoid inbreeding lol 
            while cmp_genome(parent_a, parent_b) && j < 100
                parent_b = rand(population)
                j+=1
            end
            if rand() <= fitness(parent_b)
                break
            end
            parent_b = rand(population)
        end
        
        mating_pool[i] = (parent_a => parent_b)
    end

    next_generation = Vector{S}(undef, mating_pool_size)
    for i in 1:mating_pool_size
        a, b = mating_pool[i]
        child = crossover(a,b)
        mutate!(child, mutation_rate)
        next_generation[i] = child
    end

    return next_generation
end

function check_solution(population::Vector{S}) where {S<:Genome}
    fits = [fitness(x) for x in population]
    iv = Int[]
    for (i, f) in enumerate(fits)
        if f == 1
            push!(iv, i)
        end
    end
    return population[iv]
end

function genetic_algorithm(population::Vector{S}, max_iter=100) where {S<:Genome}
    for i in 1:max_iter
        # @show i 
        # solution is found if fitness is 1
        winners = check_solution(population)
        if !isempty(winners)
            return (true, i, winners)
        end
        population = evolve(population)
    end
    return (false, max_iter, S[])
end

# =================================================
# TESTS 
# =================================================

pop_size = 100
board_size = 8

population = [randgenome(board_size) for i in 1:pop_size]
genetic_algorithm(population)

using BenchmarkTools
using Plots 

# Benchmarks 


@eval BenchmarkTools macro btimed(args...)
    _, params = prunekwargs(args...)
    bench, trial, result = gensym(), gensym(), gensym()
    trialmin, trialallocs = gensym(), gensym()
    tune_phase = hasevals(params) ? :() : :($BenchmarkTools.tune!($bench))
    return esc(quote
        local $bench = $BenchmarkTools.@benchmarkable $(args...)
        $BenchmarkTools.warmup($bench)
        $tune_phase
        local $trial, $result = $BenchmarkTools.run_result($bench)
        local $trialmin = $BenchmarkTools.minimum($trial)
        return $result, $BenchmarkTools.time($trialmin)
    end)
end

benchs = []
function bench_gen(p, cx, smp)
    smp[] = smp[] + 1
    (ok, max_iter, _) = genetic_algorithm(p)
    cx[] = cx[] + Int(ok)
end

benchs = []


for b in 1:20
    cx = Ref(0)
    smp = Ref(0)
    
    bxdata = @benchmark bench_gen(p, $cx, $smp) setup=(p=[randgenome($b) for i in 1:pop_size]) 

    println(smp[])
    println(cx[])
    
    push!(benchs, (bxdata, cx[]/smp[]))
end

benchs

gr()
plot(1:20, [median(bx).time for (bx, rate) in benchs], label="time (ns)", legend=:topleft, linecolor=:blue, markershape=:auto)
xticks!(1:20)
xlabel!("board size")
plot!(twinx(), [rate for (bx, rate) in benchs], linecolor=:orange, label="success rate", legend=:topright, markershape=:square, markercolor=:orange)

plot(rand(10), label = "Series ")

median(benchs[1][1]).time
