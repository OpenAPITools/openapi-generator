#!/bin/bash
#
# Builds a PNG file representing the graph of targets.
#
# requirements:
# - install "gprof2dot"
# - install "remake"
# - install "graphviz"

DOTFILE=Makefile.dot
PNGFILE=Makefile.png

rm -f callgrind.out.*

# Go through all non-implicit rules and get the call graph.
# Note: dry-run still executes code that is eval'd with shell macro.
for target in $(remake --targets --no-builtin-rules | sed -e 's/^[[:space:]]*//' -e '/^\./d' | grep -v '%'); do
    remake --always-make --dry-run --profile "${target}"
done

# Go through all call graph files and stitch them together,
# This generates a dot file.
echo "digraph {" > "${DOTFILE}"
for profile in callgrind.out.*; do
    echo "processing ${profile}"
    gprof2dot --format=callgrind "${profile}" | sed -e 's/digraph/subgraph/' >> "${DOTFILE}"
done
echo "}" >> "${DOTFILE}"

# Export the graph as PNG.
dot -Tpng -o "${PNGFILE}" "${DOTFILE}"
