#' # Exercise Sheet 1 - Theseus Labyrinth - Pathfinding
#' ## Abstract Definition of a Problem Solving AI Task
#' Before diving into this pathfinding problem, 
#' we define a parametric type characterizing a problem 
#' where the world is completely observable and the initial state 
#' and goal state are known. The `W` type parameter represents the type 
#' of the world and the `S` parameter represents the type of the agent states.

using DataStructures

mutable struct Problem{W, S, A}
    world::W
    initial_state::S
    goal_state::S
    all_actions::A
end

#' We then define some functions (without any concrete method), 
#' that will characterize the possible actions on our problem type.
#' Note that the `Problem` type is immutable and none of those functions 
#' alter state.

function world end 
function initial_state end 
function test_goal end 
function all_actions end 
function actions end 
function action_cost end
function apply_action end ;

#' --- 
#' ## Formulation of the Problem
#' ## PEAS formulation
#' - **P**erformance: Get Theseus to the goal cell in the shortest number of steps possible
#' - **E**nvironment: A grid labyrinth where cells can have walls on their sides
#' - **A**ctuators: Move theseus ↑, ↓, ← or → 
#' - **S**ensors: Theseus has a map of the labyrinth and knows his position
#' 
#' #### Properties of the Environment
#' - **Observability**: Completely observable 
#' - **Single/Multi-Agent**: Single Agent 
#' - **Predictability**: Completely deterministic
#' - **Episodic or Sequential**: Completely sequential 
#' - **Static or Dynamic**: Completely static. The world does not change when agent is planning the action. 
#' - **Discrete or Continuous**: Completely discrete. 
#' 
#' #### Properties of the agent 
#' The agent, Theseus, is a single agent. It is not reactive, but has a goal. 
#' It has a state, but has no utility function for goals, because in this environment we have a single goal cell.
#' 
#' We are in the field of agents for problem solving because we can entirely plan 
#' a sequence of actions before acting. 


#' We are now able to define the concrete type of problem solving task:
#' The world type, first parameter of `Problem`, is a matrix of integers,
#' While the states are defined by a tuple of two integers (coordinates in the labyrinth) 
#' The **states** are all the possible coordinates of theseus in the labyrinth in the form `(row,column)` 

const TheseusLabyrinth = Problem{Matrix{Int}, Tuple{Int, Int}, OrderedSet{Symbol}} ;


#' ### The environment: The labyrinth


const EE = 0  # Empty Cell
const LE = 1  # Left Wall 
const RI = 2  # Right Wall 
const TO = 3  # Top Wall 
const BO = 4  # Bottom Wall

# The original labyrinth in the exercise
labyrinth = 
    [
        EE EE BO EE ;
        EE TO RI EE ;
        TO RI RI EE ; 
        EE TO EE EE ; 
    ]

gen_labyrinth(rows, cols) = rand((0:4), (rows,cols))
function print_labyrinth(p::TheseusLabyrinth)
    rows, cols = size(p.world)
    printstyled(repeat("▁", cols + 2); color=:blue)
    println()
    for i in 1:rows 
        printstyled("▏";color=:blue)
        # bot lane
        for j in 1:cols
            color = if (i,j) == p.initial_state 
                :yellow 
            elseif (i,j) == p.goal_state
                :green 
            else 
                :white 
            end
            if p.world[i,j] == EE 
                if color == :yellow 
                    printstyled("θ"; color = color)
                elseif color == :green 
                    printstyled("⋆"; color = color)
                else
                    printstyled(" "; color=color)
                end
            elseif p.world[i,j] == LE
                printstyled("▏"; color=color)
            elseif p.world[i,j] == RI
                printstyled("▕"; color=color)
            elseif p.world[i,j] == TO
                printstyled("▔"; color=color)
            elseif p.world[i,j] == BO
                printstyled("▁"; color=color)
            else printstyled(m[i,j], color=color)
            end
        end
        printstyled("▕";color=:blue)
        println()
    end
    printstyled(repeat("▔", cols + 2); color=:blue)
    println()
end


#' ---
#' ## Formulation of the task as a graph search problem
#' We create an instance of the problem with initial theseus coordinates (2,1)
#' and goal coordinates (2,4)

#' ### Step 1: The initial state 
theseus_start = (2,1)
initial_state(p::TheseusLabyrinth) = p.initial_state ;

#' We need the `DataStructures.jl` package for queues and OrderedSet.
using DataStructures


#' ### Step 2: The possible actions in a state.
#' We now define a function that gives us the possible actions from a given state,
#' and define the function that gives us an ordered set of all possible actions
#' the agent can do in our world. The `actions` function works by excluding 
#' the neighbouring cells where theseus cannot move, removing the actions 
#' from the set. We will want to try out different orderings of the functions,
#' hence, we use an `OrderedSet` instead of a regular `Set`.

actions_set = OrderedSet([:↑, :←, :→, :↓])
function actions(p::TheseusLabyrinth, state) 
    y, x = state
    world = p.world
    rows, cols = size(world)
    possible_actions = all_actions(p)

    neigh_up = (y-1, x)
    neigh_down = (y+1, x)
    neigh_left = (y, x-1)
    neigh_right = (y, x+1)

    cell = world[state...]

    if !checkbounds(Bool, world, neigh_up...) || 
            cell == TO || world[neigh_up...] == BO
        delete!(possible_actions, :↑)
    end
    if !checkbounds(Bool, world, neigh_down...) || 
            cell == BO || world[neigh_down...] == TO
        delete!(possible_actions, :↓)
    end
    if !checkbounds(Bool, world, neigh_left...) || 
            cell == LE || world[neigh_left...] == RI
        delete!(possible_actions, :←)
    end
    if !checkbounds(Bool, world, neigh_right...) || 
            cell == RI || world[neigh_right...] == LE
        delete!(possible_actions, :→)
    end

    return collect(possible_actions)
end ;

#' ### Step 3: The transition model
#' We now have to define the **transition model** by defining a function $$result : State \times Action \to State$$
#' function that returns the successor state.
function apply_action(p::TheseusLabyrinth, state, action)
    y, x = state
    if action ∉ actions(p, state)
        error("cannot apply action $action in state $(state)!")
    end 
    if action == :↑
        (y-1, x)
    elseif action == :↓
        (y+1, x)
    elseif action == :←
        (y, x-1)
    elseif action == :→ 
        (y, x+1)
    else 
        error("unknown action")
    end
end

#' ### Step 4: Goal test
#' Must be a boolean function, testing if the agent has found the solution.
test_goal(p::TheseusLabyrinth, state) = (state == p.goal_state)


#' ### Step 5: Cost of the path.
#' The cost of a path is the sum of the costs of actions. Must never be negative
#' We set the cost of actions to be 1
action_cost(p::TheseusLabyrinth, state, action) = 1

#' ---
#' ## Visualizing the state space
#' The state space is implicityly defined by the initial state, the possible actions and the transition model.
#' We now define a concrete instance of the Theseus' Labyrinth problem 


problem_instance = TheseusLabyrinth(labyrinth, theseus_start, (2,4), actions_set) ;
all_actions(p::TheseusLabyrinth) = copy(p.all_actions)

#' Lets pretty print the labyrinth
print_labyrinth(problem_instance)

#' Let's build and draw the state space graph

using LightGraphs, MetaGraphs, GraphRecipes, Plots

function build_search_space(prob_inst)
    rows, cols = size(prob_inst.world)
    g = MetaGraph()
    set_indexing_prop!(g, :state)

    for x in 1:cols, y in 1:rows 
        add_vertex!(g)
        set_prop!(g, nv(g), :state, (x,y))
        set_prop!(g, nv(g), :name, repr((x,y)))
    end

    for i in 1:nv(g)
        state = g[i, :state]

        for action in actions(prob_inst, state)
            res_state = apply_action(prob_inst, state, action)
            j = g[res_state, :state]
            add_edge!(g, i, j)
        end
    end

    return g
end

function get_labels(g, prob_inst)
    [
        begin
            state = get_prop(g, i, :state)
            if state == initial_state
                "START $state"
            elseif test_goal(prob_inst, state)
                "GOAL $state"
            else 
                repr(state)
            end 
        end for i in 1:nv(g)
    ]
end


function plot_search_space(g, prob_inst)
    graphplot(g, names=get_labels(g, prob_inst), curves=false, 
    nodeshape=:rect, nodecolor=:white, nodesize=0.15,)
end

g = build_search_space(problem_instance)
plot_search_space(g, problem_instance)

#' ---
#' ## Tree Search Methods For solving the problem
#' ### Visualizing the search tree.
#' We want to visualize as a tree, the states that have been visited by our algorithms.
#' Let's define some helper function to do such.

function plot_search_tree(st, prob_inst)
    graphplot(st, names=get_labels(st, prob_inst), curves=false, method=:buchheim, root=:left,
        nodeshape=:rect, nodecolor=:white, nodesize=0.15, size=(1280, 1000), axis_buffer=0.04)    
end

#' We also define a recursive type that holds the solution path 
struct Node{S}
    state::S 
    parent::Union{Nothing, Node{S}}
end

Node(a) = Node(a, nothing)

function Base.collect(node::Node{S}) where {S} 
    curr = node
    arr = S[]
    while curr !== nothing
        push!(arr, curr.state)
        curr = curr.parent
    end
    reverse(arr)
end

#' ### Breadth First Search (Tree)
#' The Breadth First Search algorithm is a **complete** and **optimal** search algorithm,
#' but is quite expensive. It means that it always finds the solution (if existing) and 
#' always finds the shortest path to the solution, assuming that the **actions have constant cost**.
#'
#' **Time Complexity**: $$O(b^d)$$ if the the branching factor $$b$$ is fixed, 
#' where $$d$$ is the depth of the goal node closest to the initial state.
#' Alternatively, if the branching factor $$b$$ is not fixed, the worst case time complexity of BFS 
#' can be formulated as $$O(|V| + |E|)$$ where the graph $$G = \{V, E\}$$ (set of vertices and edges)
#'
#' **Space Complexity**: $$O(b^d)$$

function tree_bfs(p::Problem{W, S}) where {W, S}
    root = initial_state(p)

    # search tree
    st = MetaGraph(1)
    frontier = Queue{Tuple{Node{S}, Int}}()
    i = nv(st)
    set_prop!(st, i, :state, root)
    enqueue!(frontier, (Node(root),i))

    if (test_goal(p, root))
        return (true, root, st)
    end

    while !isempty(frontier)
        node, i = dequeue!(frontier)
        i_state = node.state
        set_prop!(st, i, :state, i_state)
        for action in actions(p, i_state)
            j_state = apply_action(p, i_state, action)
            child = Node{S}(j_state, node)

            add_vertex!(st)
            j = nv(st)
            set_prop!(st, j, :state, j_state)
            add_edge!(st, i, j)

            if test_goal(p, j_state)
                return (true, child, st)
            end
            enqueue!(frontier, (child, j))
        end
    end

    return (false, nothing, st)
end

ok, result, st = tree_bfs(problem_instance)
plot_search_tree(st, problem_instance)
collect(result)


#' ### Depth First Search (Tree)
#' The Depth First Search algorithm is a **non-optimal** search algorithm,
#' It is not complete on infinite spaces or with the **tree** method
#' This algorithm can loop indefinetly! It can be implemented as BFS by using a LIFO queue instead (a `Stack` data structure)
#' for the frontier, or can be more efficiently implemented as a recursive traversal of the graph
#' **Time Complexity**: $$O(b^d)$$ if the the branching factor $$b$$ is fixed, 
#' where $$d$$ is the depth of the goal node closest to the initial state.
#' Alternatively, if the branching factor $$b$$ is not fixed, the worst case time complexity of BFS 
#' can be formulated as $$O(|V| + |E|)$$ where the graph $$G = \{V, E\}$$ (set of vertices and edges)
#'
#' **Space Complexity**: $$O(b \cdot d)$$
function tree_dfs(p::Problem{W,S}) where {W, S}
    root = initial_state(p)
    root_node = Node(root)
    # search tree
    st = MetaGraph(1)
    i = nv(st)
    set_prop!(st, i, :state, root)
    tree_dfs_rec(p, root_node, i, st)
end

function tree_dfs_rec(p::Problem{W, S}, node::Node{S}, i, st) where {W, S}
    i_state = node.state
    if (test_goal(p, i_state))
        return (true, node, st)
    end

    set_prop!(st, i, :state, i_state)
    for action in actions(p, i_state)
        # println(action)
        # println(i_state)
        j_state = apply_action(p, i_state, action)
        child = Node{S}(j_state, node)

        add_vertex!(st)
        j = nv(st)
        set_prop!(st, j, :state, j_state)
        add_edge!(st, i, j)

        ok, res, _ = tree_dfs_rec(p, child, j, st)

        if ok
            return (true, res, st)
        end
    end

    return (false, nothing, st)
end


orders = [OrderedSet([:↑, :←, :→, :↓]), OrderedSet([:←, :↓, :→, :↑]), OrderedSet([:↑, :→, :↓, :←])]

for ordering in orders
    problem_instance.all_actions = ordering
    try 
        println("Ordering $(collect(ordering))")
        ok, result, st = tree_dfs(problem_instance)
        if ok 
            println(collect(result))
            plot_search_tree(st, problem_instance)
        else 
            println(nothing)
        end
    catch e 
        if e isa StackOverflowError
            println("Error! Has looped")
        else 
            rethrow(e)
        end
    end
end 

#' ### Depth Limited Search (Tree)
#' The Depth Limited Search algorithm is a **non-complete** and **non-optimal** search algorithm,
#' This algorithm behaves like Depth First Search but stops at a pre-defined depth limit.
#'
#' **Time Complexity**: $$O(b^l)$$
#' **Space Complexity**: $$O(b \cdot l)$$
function tree_dls(p::Problem{W,S}, limit) where {W, S}
    root = initial_state(p)
    root_node = Node(root)
    # search tree
    st = MetaGraph(1)
    i = nv(st)
    set_prop!(st, i, :state, root)
    depth = 0
    tree_dls_rec(p, root_node, i, st, depth, limit)
end

function tree_dls_rec(p::Problem{W, S}, node::Node{S}, i, st, depth, limit) where {W, S}
    i_state = node.state
    if (test_goal(p, i_state))
        return (true, node, st)
    end

    if depth > limit
        return (false, nothing, st)
    end

    set_prop!(st, i, :state, i_state)
    for action in actions(p, i_state)
        j_state = apply_action(p, i_state, action)
        child = Node{S}(j_state, node)

        add_vertex!(st)
        j = nv(st)
        set_prop!(st, j, :state, j_state)
        add_edge!(st, i, j)

        ok, res, _ = tree_dls_rec(p, child, j, st, depth+1, limit)

        if ok
            return (true, res, st)
        end
    end

    return (false, nothing, st)
end


ok, result, st = tree_dls(problem_instance, 4)
println(collect(result))
# plot_search_tree(st, problem_instance)

ok, result, st = tree_dls(problem_instance, 2)

# ===============================================================
# ===============================================================
# ===============================================================
# ===============================================================

#' ---
#' ## Graph Algorithms
#' ### The *A* Algorithms: Uniform Cost Search, Greedy Best First

#' We also define a recursive type that holds the solution path 
mutable struct WeightedNode{S}
    state::S 
    cost::Real
    parent::Union{Nothing, WeightedNode{S}}
end

WeightedNode(a) = WeightedNode(a, 0, nothing)

function Base.collect(node::WeightedNode{S}) where {S} 
    curr = node
    arr = S[]
    while curr !== nothing
        push!(arr, curr.state)
        curr = curr.parent
    end
    reverse(arr)
end

function a_search(p::Problem{W, S}, f::Function) where {W, S}
    i_state = initial_state(p)
    i_node = WeightedNode(i_state)
    # st = MetaGraph() 
    # i = nv(st)
    # set_prop!(st, i, :state, root)
    st = nothing

    frontier = PriorityQueue()
    explored = Set{S}()

    # enqueue
    frontier[i_node] = f(p, i_node)
    while !isempty(frontier)
        # node, i = dequeue!(frontier)
        node = dequeue!(frontier)
        i_state = node.state
        if test_goal(p, i_state)
            return (true, node, st)
        end

        push!(explored, i_state)

        for action in actions(p, i_state)
            j_state = apply_action(p, i_state, action)
            child = WeightedNode{S}(j_state, node.cost + action_cost(p, i_state, action), node)

            # add_vertex!(st)
            # j = nv(st)
            # set_prop!(st, j, :state, j_state)
            # add_edge!(st, i, j)
            
            if j_state ∉ explored && !haskey(frontier, j_state)
                frontier[child] = f(p, child)
            elseif haskey(frontier, j_state) && frontier[j_state] > f(child) 
                frontier[child] = f(p, child)
            end
        end
    end
    return (false, nothing, st)
end

function distmd(p::Problem{W, S}, n::WeightedNode{S}) where {W,S}
    y, x = n.state
    yg, xg = p.goal_state
    md = abs(x - xg) + abs(y - yg)
    n.cost + md
end 

function manhattan_distance(p::Problem{W, S}, n::WeightedNode{S}) where {W,S}
    y, x = n.state
    yg, xg = p.goal_state
    abs(x - xg) + abs(y - yg)
end

distmd(p::Problem{W, S}, n::WeightedNode{S}) where {W,S} = n.cost + manhattan_distance(p,n)
 
cost_only(p::Problem{W, S}, n::WeightedNode{S}) where {W,S} = n.cost

#' let's try A, UC and Greedy Best First searches on a bigger, randomly generated labyrinth
bigger_labyrinth = gen_labyrinth(10,10)
another_start = (rand(1:3), rand(1:3))
another_goal = (rand(7:10), rand(7:10))
#' if $$f(n) = g(n)$$ then *A* behaves exactly as Uniform Cost Search (Dijkstra's Algorithm)
another_instance = TheseusLabyrinth(bigger_labyrinth, another_start, another_goal, actions_set)
print_labyrinth(another_instance)
@timev ok, result, st = a_search(another_instance, cost_only)
collect(result), result.cost

#' if $$f(n) = g(n)$$ then *A* behaves exactly as Uniform Cost Search (Dijkstra's Algorithm)
another_instance = TheseusLabyrinth(bigger_labyrinth, another_start, another_goal, actions_set)
@timev ok, result, st = a_search(another_instance, distmd)
collect(result), result.cost

#' UC, *A\** and BFS are optimal search algorithms: they can all be seen as special cases of the *A* algorithm
#' - UC: $$f(n) = g(n)$$
#' - *A**: $$f(n) = g(n) + h(n)$$ where $$h(n) < h^*(n) = $$ cost of the shortest path from $$n$$ to the goal.
#' - BFS: $$f(n) = 1$$

const MAXITER = 2000
function hill_climbing(p::Problem{W,S}, f::Function) where {W,S}
    current = initial_state(p)
    curr_val = f(p, current)
    i = 0
    while i < maxiter
        println(current)
        i++
        neighbors = [apply_action(p, current, action) for action in actions(p, current)]
        if isempty(neighbors)
            break
        end
        # println(neighbors)
        v = map(x -> f(p, x), neighbors)
        # println(v)
        i = argmin(v)
        n = neighbors[i]
        # println("best is $n with cost $(v[i])")
        
        if v[i] >= curr_val
            break
        else 
            current = n
        end
    end
    return current
end


function manhattan_distance(p::TheseusLabyrinth, n::Tuple{Int,Int}) 
    y, x = n
    yg, xg = p.goal_state
    abs(x - xg) + abs(y - yg)
end

hill_climbing(problem_instance, manhattan_distance)

problem_instance.goal_state

problem_instance.world