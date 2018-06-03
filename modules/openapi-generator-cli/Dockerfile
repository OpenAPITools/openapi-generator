FROM openjdk:8-jre-alpine

ADD build/current/openapi-generator-cli.jar /opt/openapi-generator-cli/openapi-generator-cli.jar

ENTRYPOINT ["java", "-jar", "/opt/openapi-generator-cli/openapi-generator-cli.jar"]

CMD ["help"]