#!/usr/bin/env python3

# This script removes lines from stubtest whitelists, according to
# an input file. The input file has one entry to remove per line.
# Each line consists of a whitelist filename and an entry, separated
# by a colon.

# This script is used by the workflow to remove unused whitelist entries.

import sys
from collections import defaultdict
from typing import Dict, List, Set, Tuple


def main() -> None:
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} FILENAME", file=sys.stderr)
        sys.exit(1)

    to_remove = parse_input_file(sys.argv[1])
    for filename, entries in to_remove.items():
        remove_entries_from_whitelist(filename, entries)


def parse_input_file(input_file: str) -> Dict[str, Set[str]]:
    to_remove = defaultdict(set)
    with open(input_file) as f:
        for filename, wl_entry in [parse_input_line(li) for li in f if li.strip()]:
            to_remove[filename].add(wl_entry)
    return to_remove


# Returns a (filename, entry) tuple.
def parse_input_line(line: str) -> Tuple[str, str]:
    line = line.strip()
    filename, entry = line.split(":", maxsplit=1)
    return filename, entry


def remove_entries_from_whitelist(filename: str, entries: Set[str]) -> None:
    new_lines: List[str] = []
    with open(filename) as f:
        for line in f:
            entry = line.strip().split(" #")[0]
            if entry in entries:
                entries.remove(entry)
            else:
                new_lines.append(line)
    if entries:
        print(f"WARNING: The following entries were not found in '{filename}':", file=sys.stderr)
        for entry in entries:
            print(f"  * {entry}")
    with open(filename, "w") as f:
        for line in new_lines:
            f.write(line)


if __name__ == "__main__":
    main()
