defmodule Day5 do
  defstruct fresh_ranges: [], ingredients: []

  def parse_input(txt) do
    [fresh_ranges_str, ingredients_str] = String.split(txt, "\n\n")

    fresh_ranges =
      for line <- String.split(fresh_ranges_str) do
        [first, last] = String.split(line, "-")
        String.to_integer(first)..String.to_integer(last)
      end

    merged_ranges =
      Enum.reduce(fresh_ranges, [], fn range, merged ->
        {non_overlapping, overlapping} = Enum.split_with(merged, &Range.disjoint?(&1, range))

        merged_range =
          Enum.reduce(overlapping, range, fn r1, r2 ->
            min(r1.first, r2.first)..max(r1.last, r2.last)
          end)

        [merged_range | non_overlapping]
      end)
      |> Enum.sort(&(&1.first <= &2.first))

    ingredients = for line <- String.split(ingredients_str), do: String.to_integer(line)

    %Day5{fresh_ranges: merged_ranges, ingredients: ingredients}
  end

  def solve_part1(input) do
    Enum.count(input.ingredients, fn ingredient ->
      Enum.find(input.fresh_ranges, &(ingredient in &1))
    end)
  end
end

Aoc2025.Utils.run_day("day5-input.txt", &Day5.parse_input/1, &Day5.solve_part1/1)
