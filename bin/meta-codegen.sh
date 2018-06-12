#!/bin/sh

SCRIPT="$0"
echo "# START SCRIPT: $SCRIPT"

while [ -h "$SCRIPT" ] ; do
  ls=`ls -ld "$SCRIPT"`
  link=`expr "$ls" : '.*-> \(.*\)$'`
  if expr "$link" : '/.*' > /dev/null; then
    SCRIPT="$link"
  else
    SCRIPT=`dirname "$SCRIPT"`/"$link"
  fi
done

if [ ! -d "${APP_DIR}" ]; then
  APP_DIR=`dirname "$SCRIPT"`/..
  APP_DIR=`cd "${APP_DIR}"; pwd`
fi

executable="./modules/openapi-generator-cli/target/openapi-generator-cli.jar"

if [ ! -f "$executable" ]
then
  mvn -B clean package
fi

export JAVA_OPTS="${JAVA_OPTS} -XX:MaxPermSize=256M -Xmx1024M -DloggerPath=conf/log4j.properties"
ags="meta -n myClientCodegen -p com.my.company.codegen -o samples/meta-codegen/lib $@"

java $JAVA_OPTS -jar $executable $ags

mvn verify -f samples/meta-codegen/lib/pom.xml

ags2="generate -g myClientCodegen -i modules/openapi-generator/src/test/resources/2_0/petstore.json -o samples/meta-codegen/usage $@"

java $JAVA_OPTS -cp samples/meta-codegen/lib/target/myClientCodegen-openapi-generator-1.0.0.jar:$executable org.openapitools.codegen.OpenAPIGenerator $ags2
