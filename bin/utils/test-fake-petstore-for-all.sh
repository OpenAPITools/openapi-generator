#!/bin/bash
#
# A script to test all generators to ensure there's no Java exception when running it with OAS 2.0, 3.0 fake petstore spec
#

SCRIPT="$0"
echo "# START SCRIPT: ${SCRIPT}"

executable="./modules/openapi-generator-cli/target/openapi-generator-cli.jar"
logfile="/tmp/generator-fake-petstore-output.log"

for GENERATOR in $(java -jar ${executable} list --short | sed -e 's/,/\'$'\n''/g')
do
    # no longer test 2.0 spec as we migrated to 3.0 spec
    #if eval java -jar ${executable} generate -i modules/openapi-generator/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml -g ${GENERATOR} -o /tmp/openapi-generator-test-fake-petstore/2.0/${GENERATOR} > ${logfile} 2>&1; then
    #  echo "[OAS 2.0] Executed ${GENERATOR} successfully!"
    #else
    #  echo "ERROR: Failed to run '${GENERATOR}' generator. The command was:"
    #  echo "java -jar ${executable} generate -i modules/openapi-generator/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml -g ${GENERATOR} -o /tmp/openapi-generator-test-fake-petstore/2.0/${GENERATOR}"
    #  echo "ERROR: The output of the command was:"
    #  cat ${logfile}
    #  exit 1
    #fi

    if eval java -jar ${executable} generate -i modules/openapi-generator/src/test/resources/3_0/petstore-with-fake-endpoints-models-for-testing.yaml -g ${GENERATOR} -o /tmp/openapi-generator-test-fake-petstore/3.0/${GENERATOR} > ${logfile} 2>&1; then
      echo "[OAS 3.0] Executed ${GENERATOR} successfully!"
    else
      echo "ERROR: Failed to run '${GENERATOR}' generator. The command was:"
      echo "java -jar ${executable} generate -i modules/openapi-generator/src/test/resources/3_0/petstore-with-fake-endpoints-models-for-testing.yaml -g ${GENERATOR} -o /tmp/openapi-generator-test-fake-petstore/3.0/${GENERATOR}"
      echo "ERROR: The output of the command was:"
      cat ${logfile}
      exit 1
    fi
done
