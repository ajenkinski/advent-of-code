from utils import read_input_file


def parse_input(txt: str) -> list[int]:
    """Parse input, storing left rotations as negative numbers and right
    rotations as positive numbers"""
    rotations = []
    for line in txt.splitlines():
        if not line:
            continue
        num = int(line[1:])
        if line[0] == 'L':
            num = -num
        rotations.append(num)

    return rotations


def solve_day1(rotations: list[int]) -> int:
    num_zeroes = 0
    position = 50
    for rotation in rotations:
        position = (position + rotation) % 100
        if position == 0:
            num_zeroes += 1

    return num_zeroes


def solve_day2(rotations: list[int]) -> int:
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


def main():
    rotations = parse_input(read_input_file('day1-input.txt'))

    day1_solution = solve_day1(rotations)
    print(f"Solution to day 1: {day1_solution}")

    day2_solution = solve_day2(rotations)
    print(f"Solution to day 2: {day2_solution}")


if __name__ == "__main__":
    main()
