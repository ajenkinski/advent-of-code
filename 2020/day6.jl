#=
https://adventofcode.com/2020/day/6
=#

groups_input = split(read("day6-input.txt", String), r"\n{2,}")

# Part 1: count number of unique non-whitespace characters in each group
group_counts = map(gi -> length(unique(replace(gi, r"\s+" => ""))), groups_input)
part1_answer = sum(group_counts)
print("Part 1 answer = $(part1_answer)\n")

# Part 2: Divide each group into lines, and count number of characters that appear in all lines of a group
group_all_counts = map(gi -> length(intersect(split(gi)...)), groups_input)
part2_answer = sum(group_all_counts)
print("Part 2 answer = $(part2_answer)\n")

