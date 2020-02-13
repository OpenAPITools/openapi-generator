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

java -jar $executable list --docsite --include all >> docs/generators.md

echo "Wrote $(pwd)/docs/generators.md"
