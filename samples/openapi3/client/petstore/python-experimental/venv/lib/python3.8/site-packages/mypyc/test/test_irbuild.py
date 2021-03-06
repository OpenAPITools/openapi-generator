"""Test cases for IR generation."""

import os.path

from mypy.test.config import test_temp_dir
from mypy.test.data import DataDrivenTestCase
from mypy.errors import CompileError

from mypyc.common import TOP_LEVEL_NAME, IS_32_BIT_PLATFORM
from mypyc.ir.pprint import format_func
from mypyc.test.testutil import (
    ICODE_GEN_BUILTINS, use_custom_builtins, MypycDataSuite, build_ir_for_single_file,
    assert_test_output, remove_comment_lines, replace_native_int, replace_word_size
)
from mypyc.options import CompilerOptions

files = [
    'irbuild-basic.test',
    'irbuild-lists.test',
    'irbuild-dict.test',
    'irbuild-statements.test',
    'irbuild-nested.test',
    'irbuild-classes.test',
    'irbuild-optional.test',
    'irbuild-tuple.test',
    'irbuild-any.test',
    'irbuild-generics.test',
    'irbuild-try.test',
    'irbuild-set.test',
    'irbuild-str.test',
    'irbuild-strip-asserts.test',
    'irbuild-int.test',
]


class TestGenOps(MypycDataSuite):
    files = files
    base_path = test_temp_dir
    optional_out = True

    def run_case(self, testcase: DataDrivenTestCase) -> None:
        # Kind of hacky. Not sure if we need more structure here.
        options = CompilerOptions(strip_asserts='StripAssert' in testcase.name)
        """Perform a runtime checking transformation test case."""
        with use_custom_builtins(os.path.join(self.data_prefix, ICODE_GEN_BUILTINS), testcase):
            expected_output = remove_comment_lines(testcase.output)
            expected_output = replace_native_int(expected_output)
            expected_output = replace_word_size(expected_output)
            name = testcase.name
            # If this is specific to some bit width, always pass if platform doesn't match.
            if name.endswith('_64bit') and IS_32_BIT_PLATFORM:
                return
            if name.endswith('_32bit') and not IS_32_BIT_PLATFORM:
                return
            try:
                ir = build_ir_for_single_file(testcase.input, options)
            except CompileError as e:
                actual = e.messages
            else:
                actual = []
                for fn in ir:
                    if (fn.name == TOP_LEVEL_NAME
                            and not name.endswith('_toplevel')):
                        continue
                    actual.extend(format_func(fn))

            assert_test_output(testcase, actual, 'Invalid source code output',
                               expected_output)
