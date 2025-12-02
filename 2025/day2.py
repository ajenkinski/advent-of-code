from utils import read_input_file


def parse_input(txt: str) -> list[tuple[int, int]]:
    """Parse the input text into a list of (int, int) tuples representing inclusive ranges"""
    return [
        (int(a), int(b))
        for pair in txt.strip().split(",")
        for a, b in [pair.split("-")]
    ]


def solve_part1(pairs: list[tuple[int, int]]) -> int:
    """
    For each range, find numbers within that range that consist of a sequence of digits
    repeated twice, and return the sum of all such numbers.
    """
    answer = 0
    for a, b in pairs:
        a_len = len(str(a))
        b_len = len(str(b))
        # loop over even lengths only
        for num_digits in range(a_len + a_len % 2, b_len + 1, 2):
            low = max(a, 10 ** (num_digits - 1))
            high = min(b, 10**num_digits - 1)

            # Find lowest candidate digit sequence, i.e. lowest number like <sequence><sequence> >= low
            low_str = str(low)
            low_sequence = int(low_str[: num_digits // 2])
            if low_sequence < int(low_str[num_digits // 2 :]):
                low_sequence += 1

            # Highest candidate digit sequence, .e. <sequence><sequence> <= high
            high_str = str(high)
            high_sequence = int(high_str[: num_digits // 2])
            if high_sequence > int(high_str[num_digits // 2 :]):
                high_sequence -= 1

            for s in range(low_sequence, high_sequence + 1):
                answer += int(str(s) + str(s))

    return answer


def main():
    pairs = parse_input(read_input_file("day2-input.txt"))

    part1_solution = solve_part1(pairs)
    print(f"Part 1 solution = {part1_solution}")


if __name__ == "__main__":
    main()
