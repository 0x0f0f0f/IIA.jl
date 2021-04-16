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

EE = 0  # Empty Cell
LE = 1  # Left Wall 
RI = 2  # Right Wall 
TO = 3  # Top Wall 
BO = 4  # Bottom Wall

# The labirinth
world = 
    [
        EE EE EE EE ;
        EE TO TO LE ;
        TO RI RI EE ; 
        EE TO EE EE ; 
    ]

#' ### Formulation of the problem as a state search problem 
#' The **states** are all the possible coordinates of theseus in the labirinth in the form `(x,y)` 
initial_state = (2,1)
goal_test(state) = (state == (2,4))
all_actions = Set([:↑, :←, :→, :↓])

#' We now define a function that gives us the possible actions from a given state
function actions(state) 
    x, y = state
    rows, cols = size(world)
    possible_actions = copy(all_actions)

    neigh_up = (x, y-1)
    neigh_down = (x, y+1)
    neigh_left = (x-1, y)
    neigh_right = (x+1, y)

    if !checkbounds(Bool, world, neigh_up...) || 
            world[state...] == TO || world[neigh_up...] == BO
        delete!(possible_actions, :↑)
    end
    if !checkbounds(Bool, world, neigh_down...) || 
            world[state...] == BO || world[neigh_up...] == TO
        delete!(possible_actions, :↓)
    end
    if !checkbounds(Bool, world, neigh_left...) || 
            world[state...] == LE || world[neigh_up...] == RI
        delete!(possible_actions, :←)
    end
    if !checkbounds(Bool, world, neigh_right...) || 
            world[state...] == RI || world[neigh_up...] == LE
        delete!(possible_actions, :→)
    end

    return collect(possible_actions)
end


#' We set the cost of actions to be 1
action_cost(action) = 1

#' We now have to define the **transition model** by defining a function $$result : State \times Action \to State$$
#' function that returns the successor state.
function result(state, action)
    x, y = state
    if action ∉ possible_actions(state)
        error("cannot apply this action!")
    end 
    if action == :↑
        (x, y-1)
    elseif action == :↓
        (x, y+1)
    elseif action == :←
        (x-1, y)
    elseif action == :→ 
        (x+1, y)
    else 
        error("unknown action")
    end
end