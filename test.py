"""
Utility for automatically running the samples and comparing results
to expected results.

This takes, as its input, a file of the form:

    @file samples/linux_x86/reverse.lisp
    @compile success
    @input
    Hello, World
    @end
    @output
    dlorW ,olleH
    @end
    @error
    @empty
    @go
"""
from collections import namedtuple
import difflib
import subprocess
import traceback

import build 

FailCompile = namedtuple('FailCompile', ('exception'))
FailOutput = namedtuple('FailOutput', ('expected', 'real'))
FailError = namedtuple('FailError', ('expected', 'real'))

class CompilationTarget:
    """
    Represents an entire compilation target - a filename, along with
    the expected results of compiling it, and the IO it does.
    """
    def __init__(self, filename):
        self.filename = filename
        self.compile_success = True
        self.input = None

        # By default, these are not compared with the actual output - the
        # @output @empty or the @error @empty block is required to confirm
        # that the output is empty
        self.output = None
        self.error = None

    def execute(self):
        """
        Executes the compilation, comparing the input with the output.
        """
        try:
            binary_file = build.auto_build(self.filename)
        except build.BuildFailure as err:
            if self.compile_success:
                return FailCompile(err)
            else:
                return None

        proc = subprocess.Popen(
                [binary_file], 
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE)

        if self.input:
            stdout_bytes, stderr_bytes = proc.communicate(self.input.encode('ascii'))
        else:
            stdout_bytes, stderr_bytes = proc.communicate()

        if self.output is not None:
            stdout = stdout_bytes.decode('ascii')
            if stdout.rstrip() != self.output:
                return FailOutput(self.output, stdout)

        if self.error is not None:
            stderr = stderr_bytes.decode('ascii')
            if stderr.rstrip() != self.error:
                return FailError(self.error, stderr)

def read_to_end(fobj):
    """
    Reads lines until the line '@end' is hit.
    """
    buffer = []
    for line in fobj:
        if line.startswith('@end'):
            break

        if line.startswith('@empty'):
            return ''

        buffer.append(line.rstrip())

    return '\n'.join(buffer)

def read_target(fobj, target):
    """
    Constructs a single target from the lines in the given file.
    """
    for line in fobj:
        if not line.strip():
            continue

        if line.startswith('@compile'):
            mode = line.split()[1]
            if mode == 'fail':
                target.compile_success = False
        elif line.startswith('@input'):
            chunk = read_to_end(fobj)
            target.input = chunk
        elif line.startswith('@output'):
            chunk = read_to_end(fobj)
            target.output = chunk
        elif line.startswith('@error'):
            chunk = read_to_end(fobj)
            target.error = chunk
        elif line.startswith('@go'):
            break

def make_compilation_targets(fobj):
    """
    Generates a list of compilation targets, given a file object containing
    their definitions.
    """
    targets = []
    for line in fobj:
        if not line.strip():
            continue

        if line.startswith('@file'):
            start_idx = len('@file')
            filename = line[start_idx:].strip()

            target = CompilationTarget(filename)
            read_target(fobj, target)
            targets.append(target)

    return targets

def main():
    """
    Runs all the compilation targets defined in the samples/defs.test file.
    """
    build.init_targets()
    with open('samples/defs.test') as defs:
        targets = make_compilation_targets(defs)

    for target in targets:
        result = target.execute()
        if result is None:
            print('@success', target.filename)
        elif isinstance(result, FailCompile):
            print('@fail-compile', target.filename)

            exc_type = type(result.exception)
            tb = result.exception.__traceback__
            traceback.print_exception(exc_type, result.exception, tb)

            print('@end')
        elif isinstance(result, FailOutput):
            print('@fail-output', target.filename)

            expected_lines = result.expected.splitlines()
            real_lines = result.real.splitlines()
            for line in difflib.unified_diff(expected_lines, real_lines):
                print(line)

            print('@end')
        elif isinstance(result, FailError):
            print('@fail-error', target.filename)

            expected_lines = result.expected.splitlines()
            real_lines = result.real.splitlines()
            for line in difflib.unified_diff(expected_lines, real_lines):
                print(line)

            print('@end')


if __name__ == '__main__':
    main()
