# IDA* in Prolog
Iterative deepening A* (IDA*) is a graph traversal and path finding algorithm that can find the shortest path between the designated start node and any other member node of a weighted graph. It is a variant with iterative deepening from the A* search algorithm, in fact it borrows the idea of using a heuristic function that seeks to evaluate the remaining cost of reaching the goal. 
Unlike A*, IDA* does not use dynamic programming and therefore often ends up exploring the same nodes several times, instead it use a depth-first search algorithm so the memory usage is negligible compared of the A* algorithm. Unlike the ordinary iterative depth-first search, the IDa* it's focuses on exploring the most promising nodes and therefore does not reach the same depth everywhere in the search tree.  
While a standard depth-first search algorithm uses the search depth as a limit for each iteration, IDA* uses a more useful function F(n) = g(n) + h(n). In which, $g(n)$ represents the cost of travelling from the initial node to the current node $n$, while $h(n)$ is a heuristic specific to the problem being solved, which attempts to estimate the cost between the current node $n$ and the target node. The function $F$, within the algorithm acts as a threshold to eliminate some unpromising branches, thus avoiding to explore those branches that seem to have too long a path according to $F$.  
Specifically, the IDA* at each iteration performs a deep search, cutting the branches that exceed the threshold set by the function $F(n)$. This threshold, however, is modified, or rather increased, at each iteration by adding the minimum weight of the arc among the new arcs that have exceeded the current threshold. This means that at each iteration the threshold increases only by the value necessary to include the cost of the minimum path found so far, to which the next cost will be added.  
#

## Some implementation details
The implementation has been done in the most classical way as far as the Prolog language is concerned, i.e. we have an input rule which has the two input parameters that are the start and end node and then the other two output variables, which are the Path and the Cost respectively.   

The input graph must be manually inserted into the file indicating only the edges that make up the graph. These are stored following a simple structure containing not only the start and end node but also the weight of the arc itself. We need only the edges because it is these that influence the choices made by the algorithm (but this depends on the problem modelled by the graph).  
The arcs are all directional but it is possible to indicate the bi-directionality of a single arc by inserting another inverse arc. Or, if all the arcs are bidirectional, it can be easily obtained by reversing the comments on lines 61/62 and 67/68

In addition to the implementation itself of the algorithm, it was preferred to develop other utility functions that might be useful during implementation, although most of these are easily used as build-in functions within SWISH. This choice is due to the fact that often, as in the case of the function of the calculation of the minimum, where the use of the infinite value is necessary, there are cases which are not considered by the build-in functions and are therefore difficult to use and then to learn the new language better.