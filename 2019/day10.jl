using DataStructures: SortedSet, SortedDict, Reverse

function main()
  input = readlines("day10-input.txt")

  # Represent problem as a graph-like data structure.  Outer structure is a Dict whose keys are (x,y) coordinates of
  # asteroids.  Values are Dict whose keys are the angles of other asteroids, and values are lists of coordinates of
  # other asteroids at that angle, sorted in ascending order by distance.

  ast_coords = [(x=x, y=y) for (y, line)=enumerate(input) for (x, ch)=enumerate(line) if ch == '#']

  graph = Dict()

  for (c1, c2) in [(c1, c2) for c1=ast_coords for c2=ast_coords if c1 != c2]
    dx = c2.x - c1.x
    dy = c2.y - c1.y
    
    angle = atand(dx, dy)
    distance = sqrt((c2.y - c1.y)^2 + (c2.x - c1.x)^2)

    angles_dict = get!(() -> SortedDict(Reverse), graph, c1)
    nbrs_list = get!(SortedSet, angles_dict, angle)
    push!(nbrs_list, (distance, c2))
  end

  # convert to array of pairs
  asteroids = collect(graph)

  # part 1:  For part 1 I just need to find the asteroid with the most neighbors at distinct angles
  part1_answer, part1_index = findmax(map(a -> length(a.second), asteroids))
  println("Part 1: $part1_answer")

  # Part 2: 
  station_coord, station_state = asteroids[part1_index]
  println("station coord = $station_coord")
  
  num_destroyed = 0
  last_destroyed_coord = nothing

  while num_destroyed < 200 && !isempty(station_state)
    for (angle, ngbrs) in collect(station_state)
      _, last_destroyed_coord = pop!(ngbrs)

      num_destroyed += 1

      # Remove angle from the dict if there are no more asteroids at this angle
      if isempty(ngbrs)
        pop!(station_state, angle)
      end

      if num_destroyed >= 200
        break
      end
    end
  end

  part2_answer = (last_destroyed_coord.x - 1) * 100 + (last_destroyed_coord.y - 1)
  println("Part 2: $part2_answer")
end

main()

