defmodule Day6 do
  defstruct columns: [], operators: []

  def parse_input(txt) do
    rows = txt |> String.trim() |> String.split("\n")
    num_rows = Enum.drop(rows, -1)
    operator_row = List.last(rows)

    # need to preserve spaces in numbers rows for part 2.  Use operator positions to 
    # find starts of columns
    col_starts = for {ch, i} <- Enum.with_index(String.graphemes(operator_row)), ch != " ", do: i

    operators =
      operator_row
      |> String.split()
      |> Enum.map(fn op ->
        if op == "+" do
          &+/2
        else
          &*/2
        end
      end)

    # transpose rows so we end up with a list of columns
    columns =
      Enum.reverse(num_rows)
      |> Enum.reduce(List.duplicate([], Enum.count(operators)), fn row, columns ->
        # split into fields on column start indexes, preserving space within columns
        fields =
          for [start, next_start] <-
                Enum.chunk_every(col_starts, 2, 1, [String.length(row) + 1]) do
            String.slice(row, start..(next_start - 2))
          end

        for {col, num_str} <- Enum.zip(columns, fields) do
          [num_str | col]
        end
      end)

    %Day6{columns: columns, operators: operators}
  end

  def solve_part1(input) do
    Enum.sum_by(Enum.zip(input.columns, input.operators), fn {col, op} ->
      Enum.reduce(Enum.map(col, fn f -> String.to_integer(String.trim(f)) end), op)
    end)
  end

  @doc """
  Part 2 wants you to treat each column as consisting of vertical columns of digits.
  """
  def solve_part2(input) do
    Enum.sum_by(Enum.zip(input.columns, input.operators), fn {col, op} ->
      Enum.map(col, &String.graphemes/1)
      |> Enum.zip()
      |> Enum.map(fn digits ->
        Tuple.to_list(digits)
        |> Enum.join()
        |> String.trim()
        |> String.to_integer()
      end)
      |> Enum.reduce(op)
    end)
  end
end

Aoc2025.Utils.run_day(
  "day6-input.txt",
  &Day6.parse_input/1,
  &Day6.solve_part1/1,
  &Day6.solve_part2/1
)
