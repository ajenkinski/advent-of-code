defmodule Day1 do
  @doc """
  Parse input to a list, with left rotations as negative numbers and right rotations
  as positive numbers.
  """
  def parse_input(txt) do
    for line <- String.split(txt),
        <<dir, num_str::binary>> = line,
        num = String.to_integer(num_str) do
      if dir == ?L do
        -num
      else
        num
      end
    end
  end

  def solve_part1(rotations) do
    {num_zeros, _} =
      Enum.reduce(rotations, {0, 50}, fn rotation, {num_zeros, position} ->
        position = Integer.mod(position + rotation, 100)

        {if position == 0 do
           num_zeros + 1
         else
           num_zeros
         end, position}
      end)

    num_zeros
  end

  def solve_part2(rotations) do
    {num_zeros, _} =
      Enum.reduce(rotations, {0, 50}, fn rotation, {num_zeros, position} ->
        num_zeros =
          if rotation > 0 do
            num_zeros + div(position + rotation, 100)
          else
            num_zeros + div(Integer.mod(100 - position, 100) - rotation, 100)
          end

        position = Integer.mod(position + rotation, 100)
        {num_zeros, position}
      end)

    num_zeros
  end
end

Aoc2025.Utils.run_day("day1-input.txt", &Day1.parse_input/1, &Day1.solve_part1/1, &Day1.solve_part2/1)
