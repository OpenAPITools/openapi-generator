FROM jimschubert/8-jdk-alpine-mvn

WORKDIR /generator

COPY target/lib/jetty-runner* /generator/jetty-runner.jar
COPY target/*.war /generator/swagger-generator.war

ENV GENERATOR_HOST=https://generator.swaggerhub.com/api/swagger.json

EXPOSE 8080

CMD ["java", "-jar", "/generator/jetty-runner.jar", "/generator/swagger-generator.war"]

