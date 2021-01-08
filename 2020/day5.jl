#=
https://adventofcode.com/2020/day/5
=#

NUM_ROWS = 128
NUM_COLS = 8

function compute_seat_id(code)
  first_row = 0
  last_row = NUM_ROWS - 1
  first_col = 0
  last_col = NUM_COLS - 1

  for ch in code
    if ch == 'F'
      last_row -= (last_row - first_row + 1) / 2
    elseif ch == 'B'
      first_row += (last_row - first_row + 1) / 2
    elseif ch == 'L'
      last_col -= (last_col - first_col + 1) / 2
    elseif ch == 'R'
      first_col += (last_col - first_col + 1) / 2
    end
  end

  @assert first_row == last_row
  @assert first_col == last_col

  return Int(last_row * NUM_COLS + last_col)
end

input = readlines("day5-input.txt")

highest_seat_id = maximum(compute_seat_id, input)
print("Part 1: max seat id = $(highest_seat_id)\n")


seat_ids = sort(map(compute_seat_id, input))

# find the gap in the sequence
missing_id = -1

for i = 1:length(seat_ids)-1
  if seat_ids[i] != seat_ids[i+1] - 1
    global missing_id = seat_ids[i] + 1
    break
  end
end

print("Part 2: Missing seat id = $(missing_id)\n")
