#=
https://adventofcode.com/2020/day/9
=#


input = parse.(Int, readlines("day9-input.txt"))

"
Returns the first number in numbers which is not the sum of 2 numbers in the previous window_size numbers.
"
function part1_calc(numbers, window_size)
  @assert window_size ∈ 2:length(numbers)-1

  for i in window_size+1:length(numbers)
    window = Set(numbers[i-window_size:i-1])

    while !isempty(window)
      a = pop!(window)
      b = numbers[i] - a
      if b ∈ window
        # a + b == numbers[i], so move on to next window
        break
      end
    end

    if isempty(window)
      return numbers[i]
    end
  end
end

part1_answer = part1_calc(input, 25)
print("Part 1: $(part1_answer)\n")


"
Find a contiguous sequence of numbers in `numbers` whose sum is `target`, returns a (start_index, end_index) tuple, or 
`nothing, nothing` if not found.
"
function part2_calc(numbers, target)
  first_idx = 1
  last_idx = 3

  while last_idx <= length(numbers)
    seq_sum = sum(numbers[first_idx:last_idx])
    if seq_sum == target
      return (first_idx, last_idx)
    elseif seq_sum < target
      last_idx += 1
    else
      first_idx += 1
      if first_idx > (last_idx - 2)
        last_idx += 1
      end
    end
  end

  return nothing, nothing
end

first_idx, last_idx = part2_calc(input, part1_answer)
@assert first_idx != nothing
part2_answer = sum(extrema(input[first_idx:last_idx]))
print("Part 2: $(part2_answer)")

