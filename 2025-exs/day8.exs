defmodule Day8 do
  defmodule Point do
    defstruct x: 0, y: 0, z: 0
  end

  def parse_input(txt) do
    String.trim(txt)
    |> String.split()
    |> Enum.map(fn line ->
      [x, y, z] = String.split(line, ",") |> Enum.map(&String.to_integer/1)
      %Point{x: x, y: y, z: z}
    end)
  end

  defp distance(p1, p2) do
    ((p1.x - p2.x) ** 2 + (p1.y - p2.y) ** 2 + (p1.z - p2.z) ** 2) ** 0.5
  end

  @spec n_combinations(pos_integer(), list()) :: [list()]
  defp n_combinations(0, _), do: [[]]
  defp n_combinations(_, []), do: []

  defp n_combinations(n, [h | t]) do
    sublists = for l <- n_combinations(n - 1, t), do: [h | l]

    sublists ++ n_combinations(n, t)
  end

  def solve_part1(points) do
    pair_distances =
      for [p1, p2] <- n_combinations(2, points) do
        {distance(p1, p2), {p1, p2}}
      end
      |> Enum.sort(fn {d1, _}, {d2, _} -> d1 <= d2 end)

    g =
      Graph.new(type: :undirected)
      |> Graph.add_edges(
        for {_, {p1, p2}} <- Stream.take(pair_distances, 1000) do
          {p1, p2}
        end
      )

    Graph.components(g)
    |> Enum.map(&Enum.count/1)
    |> Enum.sort(:desc)
    |> Enum.take(3)
    |> Enum.product()
  end
end

Aoc2025.Utils.run_day("day8-input.txt", &Day8.parse_input/1, &Day8.solve_part1/1)
