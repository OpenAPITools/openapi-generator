"""
- make diff-mk-include
# export UPDATE_MK_INCLUDE to true
- make diff-mk-include
# test chore-update-mk-include branch condition

"""
import pytest

import structlog
structlog.configure(logger_factory=structlog.stdlib.LoggerFactory())

from tests.test_utils import *


def test_create_pr():
    output = run_cmd("make diff-mk-include")
    assert_in_output(output, ["gh pr create -B master -b update mk-include -t chore: update mk-include"])

def test_newest_mk_include():
    output = run_cmd("make diff-mk-include")
    assert_in_output(output, ["mk-include is already at newest pinned version"])

def test_dirty_git_tree():
    output = run_cmd("make -f Makefile_auto_update_disabled update-mk-include")
    assert_in_output(output, ["git must be clean to update mk-include"])

def test_disable_auto_update():
    output = run_cmd("make -f Makefile_auto_update_disabled diff-mk-include")
    assert_in_output(output, ["auto update mk-include is disabled"])
