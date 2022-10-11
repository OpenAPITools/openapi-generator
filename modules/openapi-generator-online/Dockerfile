FROM openjdk:8-jre-alpine

WORKDIR /generator

COPY target/openapi-generator-online.jar /generator/openapi-generator-online.jar

# GENERATOR_HOST can be used to determine the target location of a download link.
# The default value assumes binding to host via: docker -p 8080:8080 image_name
# Generally, this "just works" without GENERATOR_HOST, and this is provided only as
# a workaround if all else fails.
ENV GENERATOR_HOST=""

EXPOSE 8080

CMD ["java", "-jar", "/generator/openapi-generator-online.jar" ]
