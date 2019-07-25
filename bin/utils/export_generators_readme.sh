#!/bin/sh

SCRIPT="$0"
echo "# START SCRIPT: $SCRIPT"

executable="./modules/openapi-generator-cli/target/openapi-generator-cli.jar"

\rm -rf docs/generators.md

cat > docs/generators.md << EOF
---
id: generators
title: Generators List
---

EOF

java -jar $executable list | sed -e 's/\([A-Z]*\) generators:/* \1 generators:/g' -e 's/- \([a-z0-9\-]*\)/- [\1]\(generators\/\1.md\)/g' >> docs/generators.md

echo "Wrote $(pwd)/docs/generators.md"
