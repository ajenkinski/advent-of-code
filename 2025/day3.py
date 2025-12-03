from utils import run_day


def parse_input(txt: str) -> list[str]:
    return list(txt.splitlines())


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


def solve_day1(banks: list[str]) -> int:
    return solve(banks, 2)


def solve_day2(banks: list[str]) -> int:
    return solve(banks, 12)


if __name__ == "__main__":
    run_day("day3-input.txt", parse_input, solve_day1, solve_day2)
