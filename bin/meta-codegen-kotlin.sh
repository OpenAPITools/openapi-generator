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
  ./mvnw -B clean package
fi

\rm -rf "samples/meta-codegen-kotlin/lib"

export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties"
ags="meta -n myClientCodegen -t DOCUMENTATION -p com.my.company.codegen -o samples/meta-codegen-kotlin/lib -l kotlin $@"

java $JAVA_OPTS -jar $executable $ags

if [ ! -f samples/meta-codegen-kotlin/gradle/wrapper/gradle-wrapper.jar ]; then
  (cd samples/meta-codegen-kotlin/ && gradle wrapper --gradle-version 5.6.2 --distribution-type bin)
fi


(cp samples/meta-codegen-kotlin/gradlew samples/meta-codegen-kotlin/lib/ && \
 cp -R samples/meta-codegen-kotlin/gradle samples/meta-codegen-kotlin/lib/ && \
  cd samples/meta-codegen-kotlin/lib && \
  ./gradlew shadowJar)

ags2="generate -g myClientCodegen -i modules/openapi-generator/src/test/resources/2_0/petstore.json -o samples/meta-codegen-kotlin/usage $@"

java $JAVA_OPTS -cp samples/meta-codegen-kotlin/lib/build/libs/my-client-codegen-openapi-generator-1.0-SNAPSHOT-all.jar:$executable org.openapitools.codegen.OpenAPIGenerator $ags2
