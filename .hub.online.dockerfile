## The builder labeled image acts as a transient container which is meant to
## hold all non-artifact code.
##
## You can build _just_ this part with:
##     docker --target builder -t container-name:builder -f .hub.online.dockerfile .
FROM jimschubert/8-jdk-alpine-mvn:1.0 as builder

RUN set -x && \
    apk add --no-cache bash

ENV GEN_DIR /opt/openapi-generator
WORKDIR ${GEN_DIR}
COPY . ${GEN_DIR}

# Pre-compile openapi-generator-online
RUN mvn -am -pl "modules/openapi-generator-online" package

## The final (release) image
## The resulting container here only needs the target jar
FROM openjdk:8-jre-alpine

ENV GEN_DIR /opt/openapi-generator
ENV TARGET_DIR /generator

RUN mkdir -p ${TARGET_DIR}

WORKDIR ${TARGET_DIR}

COPY --from=builder ${GEN_DIR}/modules/openapi-generator-online/target/openapi-generator-online.jar ${TARGET_DIR}/openapi-generator-online.jar

ENV GENERATOR_HOST=http://localhost

EXPOSE 8080

CMD ["java", "-jar", "/generator/openapi-generator-online.jar"]
