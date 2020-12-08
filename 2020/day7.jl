#=
https://adventofcode.com/2020/day/7
=#

# Represent input as a directed graph with weighted edges.  Node names are the bag colors, 
# edge weights are required number of bags.  Node A has an edge to B if bag color A contains one or more Bs.

using SparseArrays

struct Edge
  from_node::String
  to_node::String
  count::Int
end

nodes = map(readlines("day7-input.txt")) do line
  m = match(r"^(.+) bags contain (.+)$", line)
  @assert m != nothing
  node_color = m.captures[1]
  
  ngbrs_str = m.captures[2]
  ngbrs = map(eachmatch(r"(\d+) (.*?) bag", ngbrs_str)) do ngbr_m
    Edge(node_color, ngbr_m.captures[2], parse(Int, ngbr_m.captures[1]))
  end

  return (node_color, ngbrs)
end

# map node label to indexes
label_to_idx = Dict(map(((i, node),) -> (node[1], i), enumerate(nodes)))

# Represent graph as a sparse matrix, where g[r, c] being non-zero means there's an edge from node r to node c,
# or for this problem, it means a bag of color r contains one or more bags of color c.
graph = let edges = Iterators.flatten(map(n -> n[2], nodes))
  sparse(map(e -> label_to_idx[e.from_node], edges), # row indexes
         map(e -> label_to_idx[e.to_node], edges), # column indexes
         map(e -> e.count, edges), # values
         length(nodes), length(nodes))
end

"Return an array of node indexes of all nodes in graph g reachable from node start"
function reachable_nodes(g, start)
  reachable = Set{Int}()
  to_visit = findnz(g[start, :])[1]

  while !isempty(to_visit)
    union!(reachable, to_visit)
    to_visit = setdiff(findnz(g[to_visit, :])[2], reachable)
  en

  return reachable
end


# Part 1 asks to how many different bag colors can directly or indirectly contain a "shiny gold" bag.
# For this I want to find all ancestor nodes of the shiny gold node.  Since my graph edges express the "contains" relation, I'll 
# transpose the graph so the edges express "contained by".

part1_node_ids = reachable_nodes(transpose(graph), label_to_idx["shiny gold"])
print("Part 1: shiny gold is contained by $(length(part1_node_ids)) bags\n")


# Part 2 requires counting how many bags are in a "shiny gold" bag, or in graph terms, finding all reachable nodes and summing
# the edge weights along the way.

part2_node_ids = reachable_nodes(graph, label_to_idx["shiny gold"])
print("Part 2: shiny gold contains $(length(part2_node_ids)) bags\n")

