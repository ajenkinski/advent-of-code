from utils import read_input_file


def solve_part1(banks: list[str]) -> int:
    """
    Each bank consists of a string a digits. For each bank, find the largest two
    digit number that can be made by choosing two digits from the bank, preserving
    their order.  Return the sum of these numbers.
    """

    answer = 0

    for bank in banks:
        first_digit = 0
        second_digit = 0

        for i, digit in enumerate(map(int, bank)):
            if i < (len(bank) - 1) and digit > first_digit:
                first_digit = digit
                second_digit = 0
            else:
                second_digit = max(second_digit, digit)

        answer += first_digit * 10 + second_digit

    return answer


def main():
    banks = list(read_input_file("day3-input.txt").splitlines())

    part1_solution = solve_part1(banks)
    print(f"Part 1 solution = {part1_solution}")


if __name__ == "__main__":
    main()
