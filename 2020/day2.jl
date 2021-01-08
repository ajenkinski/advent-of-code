#=
https://adventofcode.com/2020/day/2

Part 1
Suppose you have the following list:

1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
Each line gives the password policy and then the password. The password policy indicates the lowest and highest number of times a given letter must appear for the password to be valid. For example, 1-3 a means that the password must contain a at least 1 time and at most 3 times.

In the above example, 2 passwords are valid. The middle password, cdefg, is not; it contains no instances of b, but needs at least 1. The first and third passwords are valid: they contain one a or nine c, both within the limits of their respective policies.

How many passwords are valid according to their policies?

Part 2:

The shopkeeper suddenly realizes that he just accidentally explained the password policy rules from his old job at the sled rental place down the street! The Official Toboggan Corporate Policy actually works a little differently.

Each policy actually describes two positions in the password, where 1 means the first character, 2 means the second character, and so on. (Be careful; Toboggan Corporate Policies have no concept of "index zero"!) Exactly one of these positions must contain the given letter. Other occurrences of the letter are irrelevant for the purposes of policy enforcement.

Given the same example list from above:

1-3 a: abcde is valid: position 1 contains a and position 3 does not.
1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.
How many passwords are valid according to the new interpretation of the policies?
=#

struct Entry
  num1::Int
  num2::Int
  char::Char
  password::String
end

function is_valid_part1(entry)
  num_chars = count(c -> c == entry.char, entry.password)
  return num_chars in entry.num1:entry.num2
end

function is_valid_part2(entry)
  len = length(entry.password)

  return xor(
    len >= entry.num1 && entry.password[entry.num1] == entry.char,
    len >= entry.num2 && entry.password[entry.num2] == entry.char,
  )
end

# Parse input file into an array of Entry objects
input = map(readlines("day2-input.txt")) do line
  m = match(r"^(\d+)-(\d+) (.): (.+)$", line)

  num1 = parse(Int, m.captures[1])
  num2 = parse(Int, m.captures[2])
  char = m.captures[3][1]
  password = m.captures[4]

  return Entry(num1, num2, char, password)
end



print("Part 1: Num valid = $(count(is_valid_part1, input))\n")

print("Part 2: Num valid = $(count(is_valid_part2, input))\n")
