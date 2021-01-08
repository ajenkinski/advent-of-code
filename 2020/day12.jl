mutable struct Part1State
  x::Int
  y::Int
  heading::Int
end

function parse_instruction(inst)
  m = match(r"^([NSEWLRF])(\d+)$", inst)
  @assert m != nothing
  code, amount = m[1], parse(Int, m[2])

  # Assume headings are multiple of 90
  if code in ["L", "R"]
    @assert amount % 90 == 0
  end
  return code, amount
end

"Mutates state according to instruction"
function exec_instruction!(state::Part1State, instruction)
  code, amount = parse_instruction(instruction)

  if code == "N"
    state.y += amount
  elseif code == "S"
    state.y -= amount
  elseif code == "E"
    state.x += amount
  elseif code == "W"
    state.x -= amount
  elseif code == "L"
    state.heading = (state.heading - amount + 360) % 360
  elseif code == "R"
    state.heading = (state.heading + amount) % 360
  elseif code == "F"
    direction_code = ["N", "E", "S", "W"][state.heading÷90+1]
    exec_instruction!(state, "$direction_code$amount")
  else
    error("Unexpected instruction: $instruction")
  end
end

mutable struct Part2State
  # Ship position
  sx::Int
  sy::Int

  # Waypoint position
  wx::Int
  wy::Int
end

function rotate_point(x, y, degrees)
  s = sind(degrees)
  c = cosd(degrees)
  nx = round(Int, x * c - y * s)
  ny = round(Int, y * c + x * s)
  return nx, ny
end

"Mutates state according to instruction"
function exec_instruction!(state::Part2State, instruction)
  code, amount = parse_instruction(instruction)

  if code == "N"
    state.wy += amount
  elseif code == "S"
    state.wy -= amount
  elseif code == "E"
    state.wx += amount
  elseif code == "W"
    state.wx -= amount
  elseif code == "L"
    state.wx, state.wy = rotate_point(state.wx, state.wy, amount)
  elseif code == "R"
    state.wx, state.wy = rotate_point(state.wx, state.wy, -amount)
  elseif code == "F"
    state.sx += state.wx * amount
    state.sy += state.wy * amount
  else
    error("Unexpected instruction: $instruction")
  end
end


function main()
  input = readlines("day12-input.txt")

  # part 1
  # Ship starts at position 0, facing east
  state1 = Part1State(0, 0, 90)
  for line in input
    exec_instruction!(state1, line)
  end

  part1_answer = abs(state1.x) + abs(state1.y)
  println("Part 1: $part1_answer")

  state2 = Part2State(0, 0, 10, 1)
  for line in input
    exec_instruction!(state2, line)
  end

  part2_answer = abs(state2.sx) + abs(state2.sy)
  println("Part 2: $part2_answer")
end

main()
