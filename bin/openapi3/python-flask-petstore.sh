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
  mvn clean package
fi

# if you've executed sbt assembly previously it will use that instead.
input=modules/openapi-generator/src/test/resources/3_0/petstore.yaml
out_folder=samples/server/openapi3/petstore/python-flask
resources=modules/openapi-generator/src/main/resources/python-flask

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties"
ags="generate -t $resources -i $input -g python-flask -o $out_folder -Dservice $@"

rm -rf $out_folder/.openapi*
rm -rf $out_folder/openapi_server
rm $out_folder/.dockerignore
rm $out_folder/.gitignore
rm $out_folder/.travis.yml
rm $out_folder/Dockerfile
rm $out_folder/git_push.sh
rm $out_folder/README.md
rm $out_folder/requirements.txt
rm $out_folder/setup.py
rm $out_folder/test-requirements.txt
rm $out_folder/tox.ini

java $JAVA_OPTS -jar $executable $ags
