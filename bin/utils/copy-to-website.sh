#!/bin/sh

SCRIPT="$0"
echo "# START SCRIPT: $SCRIPT"

conduct_in=CODE_OF_CONDUCT.md
contrib_in=CONTRIBUTING.md
conduct_out=docs/conduct.md
contrib_out=docs/contributing.md


\rm -rf "${conduct_out}"
\rm -rf "${contrib_out}"

cat > "${conduct_out}" << EOF
---
id: code-of-conduct
title: Code of Conduct
---

$(tail -n +3 "${conduct_in}")
EOF
echo "Wrote $(pwd)/${conduct_out}"

cat > "${contrib_out}" << EOF
---
id: contributing
title: Guidelines For Contributing
sidebar_label: Guidelines
---

$(tail -n +3 "${contrib_in}")
EOF
echo "Wrote $(pwd)/${contrib_out}"