import os


def read_input_file(filename: str) -> str:
    """Return the contents of an input file as a string"""
    mydir = os.path.dirname(__file__)
    input_file = os.path.join(mydir, 'inputs', filename)
    with open(input_file) as f:
        return f.read()
