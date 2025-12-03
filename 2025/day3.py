from utils import read_input_file


def solve(banks: list[str], num_digits: int) -> int:
    """
    Each bank consists of a string a digits. For each bank, find the largest
    number that can be made by choosing num_digits digits from the bank, preserving
    their order.  Return the sum of these numbers.
    """

    answer = 0

    for bank in banks:
        digits = ["0"] * num_digits
        bank_len = len(bank)

        for i, digit in enumerate(bank):
            for j in range(len(digits)):
                if i < (bank_len - (num_digits - j - 1)) and digit > digits[j]:
                    digits[j] = digit
                    for k in range(j + 1, len(digits)):
                        digits[k] = "0"
                    break

        answer += int("".join(digits))

    return answer


def main():
    banks = list(read_input_file("day3-input.txt").splitlines())

    part1_solution = solve(banks, 2)
    print(f"Part 1 solution = {part1_solution}")

    part2_solution = solve(banks, 12)
    print(f"Part 2 solution = {part2_solution}")


if __name__ == "__main__":
    main()
