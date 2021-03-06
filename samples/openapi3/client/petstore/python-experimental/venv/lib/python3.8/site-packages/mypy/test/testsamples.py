"""Self check mypy package"""
import sys
import os.path
from typing import List, Set

from mypy.test.helpers import Suite, run_mypy


class TypeshedSuite(Suite):
    def check_stubs(self, version: str, *directories: str) -> None:
        if not directories:
            directories = (version,)
        for stub_type in ['stdlib', 'third_party']:
            for dir in directories:
                seen = {'__builtin__'}  # we don't want to check __builtin__, as it causes problems
                modules = []
                stubdir = os.path.join('typeshed', stub_type, dir)
                for f in find_files(stubdir, suffix='.pyi'):
                    module = file_to_module(f[len(stubdir) + 1:])
                    if module not in seen:
                        seen.add(module)
                        modules.extend(['-m', module])

                if modules:
                    run_mypy(['--python-version={}'.format(version)] + modules)

    def test_2(self) -> None:
        self.check_stubs("2.7", "2", "2and3")

    def test_3(self) -> None:
        sys_ver_str = '.'.join(map(str, sys.version_info[:2]))
        self.check_stubs(sys_ver_str, "3", "2and3")

    def test_34(self) -> None:
        self.check_stubs("3.4")

    def test_35(self) -> None:
        self.check_stubs("3.5")

    def test_36(self) -> None:
        self.check_stubs("3.6")

    def test_37(self) -> None:
        self.check_stubs("3.7")


class SamplesSuite(Suite):
    def test_samples(self) -> None:
        for f in find_files(os.path.join('test-data', 'samples'), suffix='.py'):
            mypy_args = ['--no-strict-optional']
            if f == os.path.join('test-data', 'samples', 'crawl2.py'):
                # This test requires 3.5 for async functions
                mypy_args.append('--python-version=3.5')
            run_mypy(mypy_args + [f])

    def test_stdlibsamples(self) -> None:
        seen = set()  # type: Set[str]
        stdlibsamples_dir = os.path.join('test-data', 'stdlib-samples', '3.2', 'test')
        modules = []  # type: List[str]
        for f in find_files(stdlibsamples_dir, prefix='test_', suffix='.py'):
            if f not in seen:
                seen.add(f)
                modules.append(f)
        if modules:
            # TODO: Remove need for --no-strict-optional
            run_mypy(['--no-strict-optional', '--platform=linux'] + modules)


def find_files(base: str, prefix: str = '', suffix: str = '') -> List[str]:
    return [os.path.join(root, f)
            for root, dirs, files in os.walk(base)
            for f in files
            if f.startswith(prefix) and f.endswith(suffix)]


def file_to_module(file: str) -> str:
    rv = os.path.splitext(file)[0].replace(os.sep, '.')
    if rv.endswith('.__init__'):
        rv = rv[:-len('.__init__')]
    return rv
