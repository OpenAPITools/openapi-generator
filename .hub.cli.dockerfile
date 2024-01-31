## The builder labeled image acts as a transient container which is meant to
## hold all non-artifact code.
##
## You can build _just_ this part with:
##     docker --target builder -t container-name:builder -f .hub.cli.dockerfile .
FROM maven:3-eclipse-temurin-17 as builder

ENV GEN_DIR /opt/openapi-generator
WORKDIR ${GEN_DIR}
COPY . ${GEN_DIR}

# Pre-compile openapi-generator-cli
RUN mvn -B -am -pl "modules/openapi-generator-cli" package

## The final (release) image
## The resulting container here only needs the target jar
## and ca-certificates (to be able to query HTTPS hosted specs)
FROM eclipse-temurin:17-jre

ENV GEN_DIR /opt/openapi-generator

RUN mkdir -p ${GEN_DIR}/modules/openapi-generator-cli/target

WORKDIR ${GEN_DIR}/modules/openapi-generator-cli/target

COPY --from=builder ${GEN_DIR}/modules/openapi-generator-cli/target/openapi-generator-cli.jar ${GEN_DIR}/modules/openapi-generator-cli/target/openapi-generator-cli.jar

COPY docker-entrypoint.sh /usr/local/bin/

ENTRYPOINT ["docker-entrypoint.sh"]

CMD ["help"]
