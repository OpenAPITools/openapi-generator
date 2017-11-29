#!/bin/sh

SCRIPT="$0"

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

executable="./modules/swagger-codegen-cli/target/swagger-codegen-cli.jar"

if [ ! -f "$executable" ]
then
  mvn clean package
fi

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -XX:MaxPermSize=256M -Xmx1024M -DloggerPath=conf/log4j.properties"
ags="$@ generate -t modules/swagger-codegen/src/main/resources/swift4 -i modules/swagger-codegen/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml -l swift4 -c ./bin/swift4-petstore.json -o samples/client/petstore/swift4/default"

echo "#### Petstore Swift API client (default) ####"
java $JAVA_OPTS -jar $executable $ags

ags="$@ generate -t modules/swagger-codegen/src/main/resources/swift4 -i modules/swagger-codegen/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml -l swift4 -c ./bin/swift4-petstore-promisekit.json -o samples/client/petstore/swift4/promisekit"
echo "#### Petstore Swift API client (promisekit) ####"
java $JAVA_OPTS -jar $executable $ags

ags="$@ generate -t modules/swagger-codegen/src/main/resources/swift4 -i modules/swagger-codegen/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml -l swift4 -c ./bin/swift4-petstore-rxswift.json -o samples/client/petstore/swift4/rxswift"
echo "#### Petstore Swift API client (rxswift) ####"
java $JAVA_OPTS -jar $executable $ags

ags="$@ generate -t modules/swagger-codegen/src/main/resources/swift4 -i modules/swagger-codegen/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml -l swift4 -c ./bin/swift4-petstore-objcCompatible.json -o samples/client/petstore/swift4/objcCompatible"
echo "#### Petstore Swift API client (objcCompatible) ####"
java $JAVA_OPTS -jar $executable $ags

ags="$@ generate -t modules/swagger-codegen/src/main/resources/swift4 -i modules/swagger-codegen/src/test/resources/2_0/swift4Test.json -l swift4 -c ./bin/swift4-test.json -o samples/client/test/swift4/default"
echo "#### Swift4Test Swift API client (default) ####"
java $JAVA_OPTS -jar $executable $ags
