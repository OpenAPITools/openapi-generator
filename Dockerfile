
FROM maven:3-jdk-7-alpine

WORKDIR /src
VOLUME  /src
VOLUME  /root/.m2/repository

ADD . /opt/swagger-codegen

RUN cd /opt/swagger-codegen && mvn package

ENTRYPOINT ["java", "-jar", "/opt/swagger-codegen/modules/swagger-codegen-cli/target/swagger-codegen-cli.jar"]

CMD ["help"]
