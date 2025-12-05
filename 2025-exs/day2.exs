defmodule Day2 do
  require Integer

  @doc """
  Parse the input text into a list of {int, int} tuples representing inclusive ranges
  """
  def parse_input(txt) do
    for pair <- String.trim(txt) |> String.split(","),
        [a, b] = String.split(pair, "-") do
      {String.to_integer(a), String.to_integer(b)}
    end
  end

  @doc """
  For each range, find numbers within that range that consist of a sequence of digits
  repeated twice, and return the sum of all such numbers.
  """
  def solve_part1(pairs) do
    Enum.sum_by(pairs, fn {a, b} ->
      a_len = a |> Integer.to_string() |> String.length()
      b_len = b |> Integer.to_string() |> String.length()

      # Loop over even lengths only
      Enum.sum_by(Enum.filter(a_len..b_len, &Integer.is_even/1), fn num_digits ->
        low = max(a, Integer.pow(10, num_digits - 1))
        high = min(b, Integer.pow(10, num_digits) - 1)

        seq_len = div(num_digits, 2)

        # find lowest candidate digit sequence
        low_str = Integer.to_string(low)
        low_sequence = String.slice(low_str, 0..(seq_len - 1)) |> String.to_integer()

        low_sequence =
          if low_sequence < String.slice(low_str, -seq_len..-1) |> String.to_integer() do
            low_sequence + 1
          else
            low_sequence
          end

        # find highest candidate digit sequence
        high_str = Integer.to_string(high)
        high_sequence = String.slice(high_str, 0..(seq_len - 1)) |> String.to_integer()

        high_sequence =
          if high_sequence > String.slice(high_str, -seq_len..-1) |> String.to_integer() do
            high_sequence - 1
          else
            high_sequence
          end

        Enum.sum_by(low_sequence..high_sequence//1, fn s ->
          String.to_integer("#{s}#{s}")
        end)
      end)
    end)
  end

  @doc """
  Part 2 is a generalization of part 1. For each range, find numbers within that range that consist of
  a sequence of digits repeated *at least* twice, instead of *exactly* twice.
  """
  def solve_part2(pairs) do
    Enum.sum_by(pairs, fn {a, b} ->
      a_len = a |> Integer.to_string() |> String.length()

      b_len = b |> Integer.to_string() |> String.length()

      Stream.flat_map(max(2, a_len)..b_len//1, fn num_digits ->
        low = max(a, Integer.pow(10, num_digits - 1))
        high = min(b, Integer.pow(10, num_digits) - 1)
        low_str = Integer.to_string(low)
        high_str = Integer.to_string(high)

        Stream.flat_map(1..div(num_digits, 2), fn seq_len ->
          if Integer.mod(num_digits, seq_len) != 0 do
            []
          else
            # Find lowest candidate digit sequence
            low_sequence = String.slice(low_str, 0..(seq_len - 1)) |> String.to_integer()

            # Highest candidate digit sequence
            high_sequence = String.slice(high_str, 0..(seq_len - 1)) |> String.to_integer()

            for s <- low_sequence..high_sequence,
                num =
                  Integer.to_string(s)
                  |> String.duplicate(div(num_digits, seq_len))
                  |> String.to_integer(),
                num in a..b do
              num
            end
          end
        end)
      end)
      |> Stream.uniq()
      |> Enum.sum()
    end)
  end
end

Aoc2025.Utils.run_day(
  "day2-input.txt",
  &Day2.parse_input/1,
  &Day2.solve_part1/1,
  &Day2.solve_part2/1
)
