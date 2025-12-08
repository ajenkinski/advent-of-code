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
        # loop over 3-element windows of previous row and current row
        Enum.zip(
          Enum.chunk_every(["." | prev_row], 3, 1, ["."]),
          Enum.chunk_every(["." | row], 3, 1, ["."])
        )
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
end

Aoc2025.Utils.run_day("day7-input.txt", &Day7.parse_input/1, &Day7.solve_day1/1)
