#!/bin/sh

SCRIPT="$0"
echo "# START SCRIPT: $SCRIPT"

while [ -h "$SCRIPT" ] ; do
  ls=$(ls -ld "$SCRIPT")
  link=$(expr "$ls" : '.*-> \(.*\)$')
  if expr "$link" : '/.*' > /dev/null; then
    SCRIPT="$link"
  else
    SCRIPT=$(dirname "$SCRIPT")/"$link"
  fi
done

if [ ! -d "${APP_DIR}" ]; then
  APP_DIR=$(dirname "$SCRIPT")/..
  APP_DIR=$(cd "${APP_DIR}"; pwd)
fi

executable="./modules/openapi-generator-cli/target/openapi-generator-cli.jar"

if [ ! -f "$executable" ]
then
  mvn clean package
fi

export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties"
ags="$@ generate -i modules/openapi-generator/src/test/resources/3_0/petstore.yaml -t modules/openapi-generator/src/main/resources/kotlin-spring -g kotlin-spring -o samples/openapi3/server/petstore/kotlin-springboot --additional-properties=library=spring-boot,beanValidations=true,swaggerAnnotations=true,serviceImplementation=true,serializableModel=true"

echo "Cleaning previously generated files if any from samples/server/openapi3/petstore/kotlin-springboot"
rm -rf samples/server/openapi3/petstore/kotlin-springboot

echo "Generating Kotling Spring Boot server..."
java $JAVA_OPTS -jar $executable $ags
