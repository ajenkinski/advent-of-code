# https://adventofcode.com/2025/day/4

from copy import deepcopy
from typing import Iterable

from utils import run_day


def parse_input(txt: str) -> list[list[str]]:
    return [list(row) for row in txt.splitlines()]


def find_accessible_cells(grid: list[list[str]]) -> Iterable[tuple[int, int]]:
    """
    Find the (row, col) coordinates of all non-empty cells which have fewer than 4 non-empty
    neighbors.
    """
    for row_num, row in enumerate(grid):
        for col_num in range(len(row)):
            if row[col_num] != "@":
                continue

            num_neigbors = sum(
                (
                    grid[nr][nc] == "@"
                    for dr, dc in [
                        (-1, -1),
                        (-1, 0),
                        (-1, 1),
                        (0, 1),
                        (1, 1),
                        (1, 0),
                        (1, -1),
                        (0, -1),
                    ]
                    for nr, nc in [(row_num + dr, col_num + dc)]
                    if (0 <= nr < len(grid)) and (0 <= nc < len(row))
                )
            )

            if num_neigbors < 4:
                yield row_num, col_num


def solve_part1(grid: list[list[str]]) -> int:
    return len(list(find_accessible_cells(grid)))


def solve_part2(grid: list[list[str]]) -> int:
    grid = deepcopy(grid)

    answer = 0

    accessible = list(find_accessible_cells(grid))
    while len(accessible) > 0:
        answer += len(accessible)
        for r, c in accessible:
            grid[r][c] = "."
        accessible = list(find_accessible_cells(grid))

    return answer


if __name__ == "__main__":
    run_day("day4-input.txt", parse_input, solve_part1, solve_part2)
