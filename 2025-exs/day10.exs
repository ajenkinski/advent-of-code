defmodule Day10 do
  import Bitwise, only: [|||: 2, <<<: 2]

  @type machine() :: %Day10{lights: String.t(), buttons: [[integer()]], joltages: [integer()]}
  @type input() :: [machine()]

  defstruct lights: nil, buttons: nil, joltages: nil

  @spec parse_input(String.t()) :: input()
  def parse_input(txt) do
    # A line looks like
    # [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
    rx = ~r/^\[(?<lights>[^\]]+)\] (?<buttons>\(.+\)) \{(?<joltages>[^\}]+)\}/

    for line <- String.trim(txt) |> String.split("\n") do
      match = Regex.named_captures(rx, line)

      lights = match["lights"]

      buttons =
        for bs <- String.split(match["buttons"]) do
          String.slice(bs, 1..-2//1) |> String.split(",") |> Enum.map(&String.to_integer/1)
        end

      joltages = String.split(match["joltages"], ",") |> Enum.map(&String.to_integer/1)

      %Day10{lights: lights, buttons: buttons, joltages: joltages}
    end
  end

  def find_min_button_presses(machine) do
    lights_target =
      String.replace(machine.lights, ".", "0")
      |> String.replace("#", "1")
      |> String.reverse()
      |> String.to_integer(2)

    buttons =
      for b <- machine.buttons do
        for bit <- b, reduce: 0 do
          button -> button ||| 1 <<< bit
        end
      end

    Stream.iterate(0, &(&1 + 1))
    |> Stream.flat_map(fn len -> Combinatorics.product(List.duplicate(buttons, len)) end)
    |> Stream.map(fn presses -> {presses, Enum.reduce(presses, 0, &Bitwise.bxor(&1, &2))} end)
    |> Enum.find_value(fn {presses, lights} ->
      if lights == lights_target, do: Enum.count(presses), else: nil
    end)
  end

  def solve_part1(input) do
    Enum.sum_by(input, &find_min_button_presses/1)
  end
end

Aoc2025.Utils.run_day("day10-input.txt", &Day10.parse_input/1, &Day10.solve_part1/1)
