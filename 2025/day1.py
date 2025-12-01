import os.path

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
        if rotation > 0:
            num_zeroes += (position + rotation) // 100
        else:
            num_zeroes += ((100 - position) % 100 - rotation) // 100
        position = (position + rotation) % 100

    return num_zeroes


def main():
    mydir = os.path.dirname(__file__)
    with open(os.path.join(mydir, "inputs/day1-input.txt")) as f:
        rotations = parse_input(f.read())

    day1_solution = solve_day1(rotations)
    print(f"Solution to day 1: {day1_solution}")

    day2_solution = solve_day2(rotations)
    print(f"Solution to day 2: {day2_solution}")


if __name__ == "__main__":
    main()
