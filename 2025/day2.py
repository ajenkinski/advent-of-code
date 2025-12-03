from utils import run_day


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

            seq_len = num_digits // 2

            # Find lowest candidate digit sequence, i.e. lowest number like <sequence><sequence> >= low
            low_str = str(low)
            low_sequence = int(low_str[:seq_len])
            if low_sequence < int(low_str[seq_len:]):
                low_sequence += 1

            # Highest candidate digit sequence, .e. <sequence><sequence> <= high
            high_str = str(high)
            high_sequence = int(high_str[:seq_len])
            if high_sequence > int(high_str[seq_len:]):
                high_sequence -= 1

            for s in range(low_sequence, high_sequence + 1):
                answer += int(str(s) * 2)

    return answer


def solve_part2(pairs: list[tuple[int, int]]) -> int:
    """
    Part 2 is a generalization of part 1. For each range, find numbers within that range that consist of
    a sequence of digits repeated *at least* twice, instead of *exactly* twice.
    """
    answer = 0

    for a, b in pairs:
        a_len = len(str(a))
        b_len = len(str(b))

        seen = set()

        for num_digits in range(a_len, b_len + 1):
            low = max(a, 10 ** (num_digits - 1))
            high = min(b, 10**num_digits - 1)
            low_str = str(low)
            high_str = str(high)

            for seq_len in range(1, num_digits // 2 + 1):
                if num_digits % seq_len != 0:
                    continue

                # Find lowest candidate digit sequence
                low_sequence = int(low_str[:seq_len])

                # Highest candidate digit sequence
                high_sequence = int(high_str[:seq_len])

                for s in range(low_sequence, high_sequence + 1):
                    num = int(str(s) * (num_digits // seq_len))
                    if num not in seen and (a <= num <= b):
                        seen.add(num)
                        answer += num

    return answer


if __name__ == "__main__":
    run_day("day2-input.txt", parse_input, solve_part1, solve_part2)
