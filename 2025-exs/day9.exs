defmodule Day9 do
  @doc """
  Returns input as a list of %{x, y} points
  """
  def parse_input(txt) do
    String.trim(txt)
    |> String.split()
    |> Enum.map(fn line ->
      [x, y] = String.split(line, ",")
      %{x: String.to_integer(x), y: String.to_integer(y)}
    end)
  end

  defp rectangle_area(p1, p2) do
    width = abs(p1.x - p2.x) + 1
    height = abs(p1.y - p2.y) + 1
    width * height
  end

  def solve_part1(points) do
    Combinatorics.n_combinations(2, points)
    |> Enum.map(fn [p1, p2] -> rectangle_area(p1, p2) end)
    |> Enum.max()
  end
end

Aoc2025.Utils.run_day("day9-input.txt", &Day9.parse_input/1, &Day9.solve_part1/1)
