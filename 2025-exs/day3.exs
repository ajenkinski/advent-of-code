defmodule Day3 do
  def parse_input(txt) do
    String.split(txt)
  end

  @doc """
  Bank is a list of digits, represented as strings.  Returns the largest
  number that can be made by choosing num_digits digits from bank, preserving
  their order.
  """
  def solve_bank(bank, num_digits) do
    bank_len = String.length(bank)

    String.graphemes(bank)
    |> Enum.with_index()
    |> Enum.reduce(List.duplicate("0", num_digits), fn {bank_digit, i}, digits ->
      num_left = bank_len - i

      Enum.reduce_while(Enum.with_index(digits), [], fn {digit, j}, new_digits ->
        if j < num_digits - num_left or bank_digit <= digit do
          {:cont, [digit | new_digits]}
        else
          {:halt, List.duplicate("0", num_digits - j - 1) ++ [bank_digit | new_digits]}
        end
      end)
      |> Enum.reverse()
    end)
    |> Enum.join()
    |> String.to_integer()
  end

  @doc """
  Each bank consists of a string a digits. For each bank, find the largest
  number that can be made by choosing num_digits digits from the bank, preserving
  their order.  Return the sum of these numbers.
  """
  def solve(banks, num_digits) do
    Enum.sum_by(banks, &solve_bank(&1, num_digits))
  end

  def solve_part1(banks) do
    solve(banks, 2)
  end

  def solve_part2(banks) do
    solve(banks, 12)
  end
end

Aoc2025.Utils.run_day(
  "day3-input.txt",
  &Day3.parse_input/1,
  &Day3.solve_part1/1,
  &Day3.solve_part2/1
)
