#=
https://adventofcode.com/2020/day/7
=#

# Represent input as a directed graph with weighted edges.  Node names are the bag colors, 
# edge weights are required number of bags.  Node A has an edge to B if bag color A contains one or more Bs.

using LightGraphs: add_edge!, edges, dfs_tree, neighbors
using MetaGraphs: MetaDiGraph, set_prop!, set_indexing_prop!, get_prop

graph = let lines = readlines("day7-input.txt")
  nodes = map(lines) do line
    m = match(r"^(.+) bags contain (.+)$", line)
    @assert m != nothing
    m.captures
  end

  label_to_idx = Dict(map(((i, node),) -> (node[1], i), enumerate(nodes)))

  g = MetaDiGraph(length(nodes))

  for (label, ngbrs_str) in nodes
    node_idx = label_to_idx[label]
    set_prop!(g, node_idx, :color, label)

    for m in eachmatch(r"(\d+) (.*?) bag", ngbrs_str)
      add_edge!(g, node_idx, label_to_idx[m.captures[2]], :weight, parse(Int, m.captures[1]))
    end
  end

  # Makes it possible to map from color name to node index
  set_indexing_prop!(g, :color)
  g
end

"Return an array of node indexes of all nodes in graph g reachable from node start"
function reachable_nodes(g, start_color)
  start_idx = g[start_color, :color]
  return unique(map(e -> e.dst, edges(dfs_tree(g, start_idx))))
end


# Part 1 asks to how many different bag colors can directly or indirectly contain a "shiny gold" bag.
# For this I want to find all ancestor nodes of the shiny gold node.  Since my graph edges express the "contains" relation, I'll 
# reverse the graph so the edges express "contained by".

part1_node_ids = reachable_nodes(reverse(graph), "shiny gold")
print("Part 1: shiny gold is contained by $(length(part1_node_ids)) bags\n")


# Part 2 requires counting how many bags are in a "shiny gold" bag, or in graph terms, finding all reachable nodes and finding the
# sum of the cumulative product of weights along each path.

"Counts number of bags directly or indirectly contained by the bag represented by start_node"
function count_bags(g, start_node)
  ngbrs = neighbors(g, start_node)
  if isempty(ngbrs)
    return 0
  else
    return sum(ngbrs) do ngbr
      nbags = get_prop(g, start_node, ngbr, :weight)
      nbags + nbags * count_bags(g, ngbr)
    end
  end
end

part2_answer = count_bags(graph, graph["shiny gold", :color])
print("Part 2: num bags in shiny gold bag = $(part2_answer)\n")

