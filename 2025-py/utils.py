import os
from pprint import pprint
from typing import Callable, Optional


def read_input_file(filename: str) -> str:
    """Return the contents of an input file as a string"""
    mydir = os.path.dirname(__file__)
    input_file = os.path.join(mydir, 'inputs', filename)
    with open(input_file) as f:
        return f.read()


def run_day[ProblemT, SolutionT](input_filename: str, 
                                 parse_input: Callable[[str], ProblemT],
                                 solve_part1: Optional[Callable[[ProblemT], SolutionT]]=None,
                                 solve_part2: Optional[Callable[[ProblemT], SolutionT]]=None) -> tuple[ProblemT, Optional[SolutionT], Optional[SolutionT]]:
    """
    Helper function to remove some of the boilerplate for writing main methods for AOC days.
    Reads input_filename, calls parse_input on its contents, then calls solve_part1 and solve_part2 on the parsed output,
    printing the results.
    """
    input_txt = read_input_file(input_filename)
    problem = parse_input(input_txt)
    
    if not (solve_part1 or solve_part2):
        pprint(problem)

    part1_solution = part2_solution = None

    if solve_part1:
        part1_solution = solve_part1(problem)
        print(f'Part 1 solution = {part1_solution}')

    if solve_part2:
        part2_solution = solve_part2(problem)
        print(f'Part 2 solution = {part2_solution}')

    return problem, part1_solution, part2_solution
