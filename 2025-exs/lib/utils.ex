defmodule Aoc2025.Utils do
  @doc """
  Return the contents of an input file as a string.
  """
  def read_input(file_name) do
    my_dir = File.cwd!()
    file_path = Path.join([my_dir, "inputs", file_name])
    {:ok, input} = File.read(file_path)
    input
  end

  @doc """
  Helper function to remove some of the boilerplate for writing main methods for AOC days.
  Reads input_filename, calls parse_input on its contents, then calls solve_part1 and solve_part2 on the parsed output,
  printing the results.
  """
  def run_day(input_filename, parse_input, solve_part1 \\ nil, solve_part2 \\ nil) do
    input = read_input(input_filename)
    parsed_input = parse_input.(input)

    if !solve_part1 && !solve_part2 do
      IO.inspect(parsed_input, label: "Parsed input")
    end

    if solve_part1 do
      solve_part1.(parsed_input) |> IO.inspect(label: "Part 1 solution")
    end

    if solve_part2 do
      solve_part2.(parsed_input) |> IO.inspect(label: "Part 2 solution")
    end
  end
end
