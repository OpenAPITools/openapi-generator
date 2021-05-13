#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
This command line script just outputs lines with colors.
usage: /bin/some/command 2> >(./color_errors.py)
"""

import fileinput
import sys

if __name__ == "__main__":

    for line in fileinput.input():
        print('\033[31m{:^24s}\033[0m  ┃ {}'.format("--error--", line), end='', file=sys.stderr)
