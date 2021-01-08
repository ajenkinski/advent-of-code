#=
https://adventofcode.com/2020/day/4

See day4-description.txt for details
=#

required_fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

function validate_year(str, min_val, max_val)
  if !contains(str, r"^\d{4}$")
    return false
  end
  num = parse(Int, str)
  return num in min_val:max_val
end

field_validators = Dict(
  "byr" => v -> validate_year(v, 1920, 2002),
  "iyr" => v -> validate_year(v, 2010, 2020),
  "eyr" => v -> validate_year(v, 2020, 2030),
  "hgt" => v -> begin
    m = match(r"^(\d+)(cm|in)$", v)
    if m == nothing
      return false
    end
    num = parse(Int, m.captures[1])
    unit = m.captures[2]
    if unit == "cm"
      return num in 150:193
    else
      return num in 59:76
    end
  end,
  "hcl" => v -> contains(v, r"^\#[a-f0-9]{6}$"),
  "ecl" => v -> v in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"],
  "pid" => v -> contains(v, r"^\d{9}$"),
)

function has_required_fields(passport)
  all(f -> haskey(passport, f), required_fields)
end

function has_valid_fields(passport)
  all(
    ((k, v),) -> !haskey(field_validators, k) || field_validators[k](v),
    pairs(passport),
  )
end

# Convert input to an array of Dicts, where each dict is a passport.
input =
  split(read("day4-input.txt", String), r"\n{2,}") .|>
  split .|>
  e -> Dict(split.(e, ":"))

num_valid_part1 = count(has_required_fields, input)
print("Part 1: Num valid passports = $(num_valid_part1)\n")

num_valid_part2 =
  count(p -> has_required_fields(p) && has_valid_fields(p), input)
print("Part 2: Num valid passports = $(num_valid_part2)\n")
