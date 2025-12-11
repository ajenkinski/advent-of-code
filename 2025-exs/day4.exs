defmodule Day4 do
  # (row, col) coords, so I'll use a map with {row, col} keys.
  defstruct num_rows: 0, num_cols: 0, grid: Map.new()

  def parse_input(txt) do
    rows = String.split(txt) |> Enum.map(&String.graphemes/1)
    num_rows = Enum.count(rows)
    num_cols = Enum.count(List.first(rows))
    coordinates = for r <- 1..num_rows, c <- 1..num_cols, do: {r, c}

    grid = Map.new(Enum.zip(coordinates, List.flatten(rows)))

    %Day4{num_rows: num_rows, num_cols: num_cols, grid: grid}
  end

  @doc """
  Find the {row, col} coordinates of all non-empty cells which have fewer than 4 non-empty
  neighbors.
  """
  def find_accessible_cells(input) do
    directions = [
      {-1, -1},
      {-1, 0},
      {-1, 1},
      {0, 1},
      {1, 1},
      {1, 0},
      {1, -1},
      {0, -1}
    ]

    Enum.filter(input.grid, fn {_, ch} -> ch == "@" end)
    |> Enum.flat_map(fn {{r, c}, _} ->
      num_neighbors =
        Enum.count(directions, fn {dr, dc} ->
          nr = r + dr
          nc = c + dc
          nr in 1..input.num_rows and nc in 1..input.num_cols and input.grid[{nr, nc}] == "@"
        end)

      if num_neighbors < 4 do
        [{r, c}]
      else
        []
      end
    end)
  end

  def solve_part1(input) do
    Enum.count(find_accessible_cells(input))
  end

  def solve_part2(input, count \\ 0) do
    accessible = find_accessible_cells(input)

    if Enum.empty?(accessible) do
      count
    else
      new_grid = Enum.reduce(accessible, input.grid, &Map.put(&2, &1, "."))
      new_input = Map.put(input, :grid, new_grid)

      solve_part2(new_input, count + Enum.count(accessible))
    end
  end
end

Aoc2025.Utils.run_day(
  "day4-input.txt",
  &Day4.parse_input/1,
  &Day4.solve_part1/1,
  &Day4.solve_part2/1
)
