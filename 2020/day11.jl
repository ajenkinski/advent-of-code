#=
https://adventofcode.com/2020/day/11
=#

"Perform one round of transformation of seats, as described in part 1, and returns the new seating grid"
function part1_transform(grid)
  new_grid = copy(grid)
  for r in axes(grid, 1)
    for c in axes(grid, 2)
      ch = grid[r, c]
      if ch != '.'
        num_filled = count(==('#'), grid[max(1, r-1):min(size(grid, 1), r + 1), 
                                         max(1, c-1):min(size(grid, 2), c + 1)])
        if ch == 'L' && num_filled == 0
          new_grid[r, c] = '#'
        elseif ch == '#' && num_filled >= 5
          new_grid[r, c] = 'L'
        end
      end
    end
  end

  return new_grid
end


"Perform one round of transformation of seats, as described in part 2, and returns the new seating grid"
function part2_transform(grid)
  # row, column index deltas to iterate in 8 directions
  deltas = [(-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1)]

  new_grid = copy(grid)
  for r in axes(grid, 1)
    for c in axes(grid, 2)
      ch = grid[r, c]
      if ch == '.'
        continue
      end

      # count number of visible occupied seats, by looking in 8 directions for a visible seat
      num_filled = count(deltas) do (rd, cd)
        rp = r + rd
        cp = c + cd
        while checkbounds(Bool, grid, rp, cp) && grid[rp, cp] == '.'
          rp += rd
          cp += cd
        end
        return checkbounds(Bool, grid, rp, cp) && grid[rp, cp] == '#'
      end

      if ch == 'L' && num_filled == 0
        new_grid[r, c] = '#'
      elseif ch == '#' && num_filled >= 5
        new_grid[r, c] = 'L'
      end
    end
  end

  return new_grid
end

function main()
  input = readlines("day11-input.txt")

  # convert input to a matrix of chars.  collect converts string to array of chars, permutedims converts each char array
  # to a row vector, and vcat concatenates the rows.

  grid = vcat(permutedims.(collect.(input))...)

  # Part 1: repeatedly apply transform until seating stabilizes, then count occupied seats
  old_grid = grid
  new_grid = part1_transform(grid)
  while new_grid != old_grid
    old_grid = new_grid
    new_grid = part1_transform(old_grid)
  end

  part1_answer = count(==('#'), new_grid)
  print("Part 1: $part1_answer occupied seats\n")

  # Part 2: same as part 1, but using part2_transform
  new_grid = part2_transform(grid)
  while new_grid != old_grid
    old_grid = new_grid
    new_grid = part2_transform(old_grid)
  end

  part2_answer = count(==('#'), new_grid)
  print("Part 2: $part2_answer occupied seats\n")
end

main()

