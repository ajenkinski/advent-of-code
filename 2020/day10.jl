#=
https://adventofcode.com/2020/day/10
=#

using LightGraphs: add_edge!, neighbors, SimpleDiGraph

input = parse.(Int, readlines("day10-input.txt"))

# Part 1

adapters = copy(input)

# add power outlet
push!(adapters, 0)

# Add device adapter
push!(adapters, maximum(adapters) + 3)

# Connect in increasing order of capacity
sort!(adapters)


adapter_diffs = diff(adapters)

part1_answer = count(==(1), adapter_diffs) * count(==(3), adapter_diffs)
print("Part 1: $(part1_answer)\n")


# Part 2

# Part 2 asks how many ways we can connect the power outlet to the device.  This can be modeled as a graph traversal
# problem. Construct a directed graph where each adapter, plus the power outlet and device, are the vertices, and there
# is an edge from A to B if adapter A can be connected to B.  Then the problem becomes: how many paths are there from
# the power outlet vertex to the device vertex.

graph = SimpleDiGraph(length(adapters))

for (i, val) in pairs(adapters)
  # Takes advantage of fact that adapters array is sorted
  j = i + 1
  while j <= length(adapters) && adapters[j] <= val + 3
    add_edge!(graph, i, j)
    j += 1
  end
end

"Count number of paths in graph from vertex s to vertex e.  Note this algorithm assumes an acyclic graph"
function count_paths(graph, s, e, cache=Dict{Int,Int}())
  if s == e
    return 1
  end

  # Avoid recomputing answer for subgraphs
  if haskey(cache, s)
    return cache[s]
  end

  ngbrs = neighbors(graph, s)

  if isempty(ngbrs)
    answer = 0
  else
    answer = sum(ngbr -> count_paths(graph, ngbr, e, cache), ngbrs)
  end

  cache[s] = answer
  return answer
end

part2_answer = count_paths(graph, 1, length(adapters))
print("Part 2: $(part2_answer)\n")

