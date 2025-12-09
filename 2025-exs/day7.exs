defmodule Day7 do
  def parse_input(txt) do
    String.trim(txt) |> String.split() |> Enum.map(&String.graphemes/1)
  end

  def solve_day1(rows) do
    [first_row | rest] = rows

    first_row =
      for ch <- first_row do
        if ch == "S", do: "|", else: ch
      end

    {num_splits, _} =
      Enum.reduce(rest, {0, first_row}, fn row, {num_splits, prev_row} ->
        # loop over 3-element windows of previous row and current row. I pad
        # each row with "p" on both ends so so the sliding window can include
        # the first and last elements as middle elements.
        Enum.zip(
          Enum.chunk_every(["p" | prev_row], 3, 1, ["p"]),
          Enum.chunk_every(["p" | row], 3, 1, ["p"])
        )
        # reverse so that I can prepend elements in reduce
        |> Enum.reverse()
        |> Enum.reduce({num_splits, []}, fn {[prev0, prev1, prev2], [cur0, cur1, cur2]},
                                            {num_splits, new_row} ->
          case cur1 do
            "."
            when prev1 == "|" or (cur0 == "^" and prev0 == "|") or (cur2 == "^" and prev2 == "|") ->
              {num_splits, ["|" | new_row]}

            "^" when prev1 == "|" ->
              {num_splits + 1, [cur1 | new_row]}

            _ ->
              {num_splits, [cur1 | new_row]}
          end
        end)
      end)

    num_splits
  end

  # helper for part 2
  defp path_weight(pred, val) do
    if pred and is_number(val), do: val, else: 0
  end

  def solve_day2(rows) do
    [first_row | rest] = rows

    # For part 2, I need to count all possible path.  To do that in one pass through the rows, I represent
    # paths using numbers instead of "|" characters.  The number represents how many paths lead to this point.
    # This lets me propagate the counts through the graph as I iterate.

    first_row =
      for ch <- first_row do
        if ch == "S", do: 1, else: ch
      end

    {num_paths, _} =
      Enum.reduce(rest, {1, first_row}, fn row, {num_paths, prev_row} ->
        # loop over 3-element windows of previous row and current row. I pad
        # each row with "p" on both ends so so the sliding window can include
        # the first and last elements as middle elements.
        Enum.zip(
          Enum.chunk_every(["p" | prev_row], 3, 1, ["p"]),
          Enum.chunk_every(["p" | row], 3, 1, ["p"])
        )
        # reverse so that I can prepend elements in reduce
        |> Enum.reverse()
        |> Enum.reduce({num_paths, []}, fn {[prev0, prev1, prev2], [cur0, cur1, cur2]},
                                           {num_paths, new_row} ->
          case cur1 do
            "."
            when is_number(prev1) or (cur0 == "^" and is_number(prev0)) or
                   (cur2 == "^" and is_number(prev2)) ->
              cell_val =
                path_weight(true, prev1) + path_weight(cur0 == "^", prev0) +
                  path_weight(cur2 == "^", prev2)

              {num_paths, [cell_val | new_row]}

            "^" when is_number(prev1) and cur0 == "." and cur2 == "." ->
              {num_paths + prev1, [cur1 | new_row]}

            _ ->
              {num_paths, [cur1 | new_row]}
          end
        end)
      end)

    num_paths
  end
end

Aoc2025.Utils.run_day(
  "day7-input.txt",
  &Day7.parse_input/1,
  &Day7.solve_day1/1,
  &Day7.solve_day2/1
)
