#' # Exercise Sheet 1 - Theseus Labirinth - Pathfinding
#' ## PEAS formulation
#' 
#' - **P**erformance: Get Theseus to the goal cell in the shortest number of steps possible
#' - **E**nvironment: A grid labirinth where cells can have walls on their sides
#' - **A**ctuators: Move theseus ↑, ↓, ← or → 
#' - **S**ensors: Theseus has a map of the labirinth and knows his position
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

#' ### The environment: The labirinth

const EE = 0  # Empty Cell
const LE = 1  # Left Wall 
const RI = 2  # Right Wall 
const TO = 3  # Top Wall 
const BO = 4  # Bottom Wall

# The labirinth
world = 
    [
        EE EE BO EE ;
        EE TO RI EE ;
        TO RI RI EE ; 
        EE TO EE EE ; 
    ]

#' ### Formulation of the problem as a state search problem 
#' The **states** are all the possible coordinates of theseus in the labirinth in the form `(row,column)` 

# initial coordinate of theseus
initial_state = (2,1)

# goal cell
goal_test(state) = (state == (2,4))

# all possible actions
all_actions = Set([:↑, :←, :→, :↓])

#' We now define a function that gives us the possible actions from a given state
function actions(state) 
    y, x = state
    rows, cols = size(world)
    possible_actions = copy(all_actions)

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
end


#' We set the cost of actions to be 1
action_cost(action) = 1

#' We now have to define the **transition model** by defining a function $$result : State \times Action \to State$$
#' function that returns the successor state.
function result(state, action)
    y, x = state
    if action ∉ actions(state)
        error("cannot apply this action!")
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

#' ## Visualizing the state space
#' The state space is implicityly defined by the initial state, the possible actions and the transition model.

#' Let's build the state space graph

using LightGraphs, GraphRecipes, Plots

rows, cols = size(world)
states = Tuple{Int, Int}[]
colors = []
g = SimpleGraph()

for x in 1:cols, y in 1:rows 
    add_vertex!(g)
    push!(states, (x,y))
    push!(colors, :white)
end

names = repr.(states)

for i in 1:nv(g)
    state = states[i]
    if state == initial_state
        names[i] = "START $(repr(states[i]))"
    end

    if goal_test(state)
        names[i] = "GOAL $(repr(states[i]))"
    end
    state_vertex_number = indexin([state], states)[1]
    for action in actions(state)
        res_state = result(state, action)
        result_vertex_number = indexin([res_state], states)[1]
        add_edge!(g, state_vertex_number, result_vertex_number)
    end
end



graphplot(g, names=names, curves=false, 
    nodeshape=:rect, nodecolor=:white, nodesize=0.15,
    method=:tree)


#' # Search algorithms
#' ## Tree Search