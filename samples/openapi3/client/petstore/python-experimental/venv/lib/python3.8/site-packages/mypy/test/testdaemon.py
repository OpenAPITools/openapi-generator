"""End-to-end test cases for the daemon (dmypy).

These are special because they run multiple shell commands.
"""

import os
import subprocess
import sys

from typing import List, Tuple

from mypy.test.config import test_temp_dir, PREFIX
from mypy.test.data import DataDrivenTestCase, DataSuite
from mypy.test.helpers import assert_string_arrays_equal, normalize_error_messages

# Files containing test cases descriptions.
daemon_files = [
    'daemon.test',
]


class DaemonSuite(DataSuite):
    files = daemon_files

    def run_case(self, testcase: DataDrivenTestCase) -> None:
        try:
            test_daemon(testcase)
        finally:
            # Kill the daemon if it's still running.
            run_cmd('dmypy kill')


def test_daemon(testcase: DataDrivenTestCase) -> None:
    assert testcase.old_cwd is not None, "test was not properly set up"
    for i, step in enumerate(parse_script(testcase.input)):
        cmd = step[0]
        expected_lines = step[1:]
        assert cmd.startswith('$')
        cmd = cmd[1:].strip()
        cmd = cmd.replace('{python}', sys.executable)
        sts, output = run_cmd(cmd)
        output_lines = output.splitlines()
        output_lines = normalize_error_messages(output_lines)
        if sts:
            output_lines.append('== Return code: %d' % sts)
        assert_string_arrays_equal(expected_lines,
                                   output_lines,
                                   "Command %d (%s) did not give expected output" %
                                   (i + 1, cmd))


def parse_script(input: List[str]) -> List[List[str]]:
    """Parse testcase.input into steps.

    Each command starts with a line starting with '$'.
    The first line (less '$') is sent to the shell.
    The remaining lines are expected output.
    """
    steps = []
    step = []  # type: List[str]
    for line in input:
        if line.startswith('$'):
            if step:
                assert step[0].startswith('$')
                steps.append(step)
                step = []
        step.append(line)
    if step:
        steps.append(step)
    return steps


def run_cmd(input: str) -> Tuple[int, str]:
    if input.startswith('dmypy '):
        input = sys.executable + ' -m mypy.' + input
    if input.startswith('mypy '):
        input = sys.executable + ' -m' + input
    env = os.environ.copy()
    env['PYTHONPATH'] = PREFIX
    try:
        output = subprocess.check_output(input,
                                         shell=True,
                                         stderr=subprocess.STDOUT,
                                         universal_newlines=True,
                                         cwd=test_temp_dir,
                                         env=env)
        return 0, output
    except subprocess.CalledProcessError as err:
        return err.returncode, err.output
