FROM jimschubert/8-jdk-alpine-mvn

RUN mkdir /opt

ADD . /opt/swagger-codegen

WORKDIR /opt/swagger-codegen

RUN mvn -am -pl "modules/swagger-codegen-cli" package && \
    mv /opt/swagger-codegen/modules/swagger-codegen-cli/target/swagger-codegen-cli.jar /opt/swagger-codegen/swagger-codegen-cli.jar && \
    mvn clean && \
    rm -rf ${MAVEN_HOME}/.m2/repository

ENTRYPOINT ["java", "-jar", "/opt/swagger-codegen/swagger-codegen-cli.jar"]

CMD ["help"]
