from utils import run_day


def parse_input(txt: str) -> list[int]:
    """Parse input, storing left rotations as negative numbers and right
    rotations as positive numbers"""
    rotations = []
    for line in txt.splitlines():
        if not line:
            continue
        num = int(line[1:])
        if line[0] == "L":
            num = -num
        rotations.append(num)

    return rotations


def solve_part1(rotations: list[int]) -> int:
    num_zeroes = 0
    position = 50
    for rotation in rotations:
        position = (position + rotation) % 100
        if position == 0:
            num_zeroes += 1

    return num_zeroes


def solve_part2(rotations: list[int]) -> int:
    num_zeroes = 0
    position = 50
    for rotation in rotations:
        # count how many times we pass zero rather than just how many times we land on it
        if rotation > 0:
            num_zeroes += (position + rotation) // 100
        else:
            num_zeroes += ((100 - position) % 100 - rotation) // 100
        position = (position + rotation) % 100

    return num_zeroes


if __name__ == "__main__":
    run_day("day1-input.txt", parse_input, solve_part1, solve_part2)
