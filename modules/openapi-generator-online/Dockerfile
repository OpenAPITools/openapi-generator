FROM openjdk:8-jre-alpine
  
WORKDIR /generator

COPY target/openapi-generator-online-1.0.0.jar /generator/openapi-generator-online-1.0.0.jar

ENV GENERATOR_HOST=http://localhost

EXPOSE 8080

CMD ["java", "-jar", "/generator/openapi-generator-online-1.0.0.jar"]

