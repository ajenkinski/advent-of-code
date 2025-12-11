defmodule Day9 do
  require Integer

  @type orient() :: :vert | :hor
  @type point() :: %{x: number(), y: number()}
  @type line() :: {point(), point()}

  @doc """
  Returns input as a list of %{x, y} points
  """
  @spec parse_input(String.t()) :: [point()]
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

  @spec line_direction(line()) :: orient()
  defp line_direction({p1, p2}) do
    if p1.x == p2.x, do: :vert, else: :hor
  end

  @spec inside?(point(), orient(), [line()]) :: boolean()
  # Return true if point is inside polygon based on traversing vertically toward
  # top
  def inside?(point, :vert, polygon) do
    num_crossings =
      Enum.reduce(polygon, 0, fn {p1, p2}, n ->
        if line_direction({p1, p2}) == :hor and p1.y < point.y and
             point.x >= min(p1.x, p2.x) and point.x <= max(p1.x, p2.x) do
          n + 1
        else
          n
        end
      end)

    Integer.is_odd(num_crossings)
  end

  # Return true if point is inside polygon based on traversing horizontally toward
  # left
  def inside?(point, :hor, polygon) do
    num_crossings =
      Enum.reduce(polygon, 0, fn {p1, p2}, n ->
        if line_direction({p1, p2}) == :vert and p1.x < point.x and
             point.y >= min(p1.y, p2.y) and point.y <= max(p1.y, p2.y) do
          n + 1
        else
          n
        end
      end)

    Integer.is_odd(num_crossings)
  end

  defp line_sign({p1, p2}) do
    n = p2.x - p1.x + (p2.y - p1.y)

    cond do
      n < 0 -> -1
      n == 0 -> 0
      n > 0 -> 1
    end
  end

  # Expand polygon outward by expand_amt
  defp expand_polygon(polygon, expand_amt) do
    # Figure out which side is the "outside", and move each line segment
    # expand_amt toward the outside.  I'll take advantage of the fact that the
    # input line segments alternate strictly between horizontal and vertical.
    {dx, dy} =
      with [{p1, p2} = line | _] = polygon do
        case line_direction(line) do
          :vert ->
            if inside?(%{x: p1.x, y: (p1.y + p2.y) / 2}, :hor, polygon) do
              {expand_amt, 0}
            else
              {-expand_amt, 0}
            end

          :hor ->
            if inside?(%{x: (p1.x + p2.x) / 2, y: p1.y}, :vert, polygon) do
              {0, expand_amt}
            else
              {0, -expand_amt}
            end
        end
      end

    # dx, dy now say how to adjust the x/y of the first line segment.  I'll adjust each line segment 
    # of the polygon, updating dx/dy when I turn a corner.
    adjustments =
      [
        {dx, dy}
        | Enum.chunk_every(polygon, 2, 1, :discard)
          |> Enum.scan({dx, dy}, fn [line1, line2], {dx, dy} ->
            if line_sign(line1) == line_sign(line2) do
              {-dy, -dx}
            else
              {dy, dx}
            end
          end)
      ]
      |> Enum.zip(polygon)

    # now adjustments[i] says how to adjust polygon[i].   Now make the
    # adjustment.  For each polygon[i], I need to adjust its endpoints by
    # adjustments[i].  Additionally, to make it meet with with its adjacent
    # segments, I need to adjust its first endpoint by adjustments[i-1] and its
    # second endpoint by adjustments[i+1].  So I need to iterate in chunks of
    # 3.

    Enum.chunk_every([List.last(adjustments) | adjustments], 3, 1, [List.first(adjustments)])
    |> Enum.map(fn [{{dx0, dy0}, _}, {{dx, dy}, {p1, p2}}, {{dx2, dy2}, _}] ->
      {
        %{x: p1.x + dx + dx0, y: p1.y + dy + dy0},
        %{x: p2.x + dx + dx2, y: p2.y + dy + dy2}
      }
    end)
  end

  def line_intersects_rectangle?({lp1, lp2}, {rp1, rp2}) do
    rleft = min(rp1.x, rp2.x)
    rright = max(rp1.x, rp2.x)
    rtop = min(rp1.y, rp2.y)
    rbottom = max(rp1.y, rp2.y)

    lleft = min(lp1.x, lp2.x)
    lright = max(lp1.x, lp2.x)
    ltop = min(lp1.y, lp2.y)
    lbottom = max(lp1.y, lp2.y)

    # Line is vertical or horizontal
    case line_direction({lp1, lp2}) do
      :hor -> lp1.y >= rtop and lp1.y <= rbottom and lright >= rleft and lleft <= rright
      :vert -> lp1.x >= rleft and lp1.x <= rright and lbottom >= rtop and ltop <= rbottom
    end
  end

  def solve_part2(points) do
    # all line segments of polygon
    polygon = Enum.chunk_every(points, 2, 1, [List.first(points)]) |> Enum.map(&List.to_tuple/1)

    # Expand outward so we can use line crossing algorithm to test rectangles
    expanded_polygon = expand_polygon(polygon, 0.1)

    Combinatorics.n_combinations(2, points)
    |> Enum.reduce(0, fn [p1, p2], max_area ->
      area = rectangle_area(p1, p2)

      if area <= max_area or
           Enum.any?(expanded_polygon, &line_intersects_rectangle?(&1, {p1, p2})) do
        max_area
      else
        area
      end
    end)
  end
end

Aoc2025.Utils.run_day(
  "day9-input.txt",
  &Day9.parse_input/1,
  &Day9.solve_part1/1,
  &Day9.solve_part2/1
)
