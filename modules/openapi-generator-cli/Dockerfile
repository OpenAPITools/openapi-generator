FROM java:8-jre-alpine

ADD target/swagger-codegen-cli.jar /opt/swagger-codegen-cli/swagger-codegen-cli.jar

ENTRYPOINT ["java", "-jar", "/opt/swagger-codegen-cli/swagger-codegen-cli.jar"]

CMD ["help"]