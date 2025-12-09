defmodule Day8 do
  defmodule Point do
    defstruct x: 0, y: 0, z: 0
  end

  defstruct points: [], pair_distances: []

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

  def parse_input(txt) do
    points =
      String.trim(txt)
      |> String.split()
      |> Enum.map(fn line ->
        [x, y, z] = String.split(line, ",") |> Enum.map(&String.to_integer/1)
        %Point{x: x, y: y, z: z}
      end)

    pair_distances =
      for [p1, p2] <- n_combinations(2, points) do
        {distance(p1, p2), {p1, p2}}
      end
      |> Enum.sort(fn {d1, _}, {d2, _} -> d1 <= d2 end)

    %Day8{points: points, pair_distances: pair_distances}
  end

  def solve_part1(input) do
    g =
      Graph.new(type: :undirected)
      |> Graph.add_edges(
        for {_, {p1, p2}} <- Stream.take(input.pair_distances, 1000) do
          {p1, p2}
        end
      )

    Graph.components(g)
    |> Enum.map(&Enum.count/1)
    |> Enum.sort(:desc)
    |> Enum.take(3)
    |> Enum.product()
  end

  def solve_part1_no_graph(input) do
    # instead of using a graph library to find connected components, collect
    # them myself as a list of MapSets which I add points to.  When a new point
    # pair spans two components I merge them together.
    Enum.reduce(Stream.take(input.pair_distances, 1000), [], fn {_, {p1, p2}}, components ->
      {with, without} =
        Enum.split_with(components, fn g -> MapSet.member?(g, p1) or MapSet.member?(g, p2) end)

      merged =
        if Enum.empty?(with) do
          MapSet.new([p1, p2])
        else
          Enum.reduce(with, &MapSet.union/2) |> MapSet.put(p1) |> MapSet.put(p2)
        end

      [merged | without]
    end)
    |> Enum.map(&MapSet.size/1)
    |> Enum.sort(:desc)
    |> Enum.take(3)
    |> Enum.product()
  end

  def solve_part2(input) do
    num_points = Enum.count(input.points)

    {p1, p2} =
      Enum.reduce_while(input.pair_distances, [], fn {_, {p1, p2}}, components ->
        {with, without} =
          Enum.split_with(components, fn c -> MapSet.member?(c, p1) or MapSet.member?(c, p2) end)

        with =
          if Enum.empty?(with) do
            MapSet.new([p1, p2])
          else
            Enum.reduce(with, &MapSet.union/2) |> MapSet.put(p1) |> MapSet.put(p2)
          end

        if MapSet.size(with) == num_points do
          {:halt, {p1, p2}}
        else
          {:cont, [with | without]}
        end
      end)

    p1.x * p2.x
  end
end

Aoc2025.Utils.run_day(
  "day8-input.txt",
  &Day8.parse_input/1,
  &Day8.solve_part1_no_graph/1,
  &Day8.solve_part2/1
)
