#=
Before you leave, the Elves in accounting just need you to fix your expense report (your puzzle input); apparently, something isn't quite adding up.

Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.

For example, suppose your expense report contained the following:

1721
979
366
299
675
1456
In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.

Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if you multiply them together?
=#

input = parse.(Int, readlines("day1-input.txt"))

for i in 1:length(input)-1
  for j in i+1:length(input)
    n1 = input[i]
    n2 = input[j]
    if n1 + n2 == 2020
      print("day1, part1: $(n1)*$(n2) = $(n1*n2)\n")
    end

    for k in j+1:length(input)
      n3 = input[k]
      if n1 + n2 + n3 == 2020
        print("day1, part2: $(n1)*$(n2)*$(n3) = $(n1*n2*n3)\n")
      end
    end
  end
end

