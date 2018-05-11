FROM jimschubert/8-jdk-alpine-mvn:1.0

RUN set -x && \
    apk add --no-cache bash

ENV GEN_DIR /opt/openapi-generator
WORKDIR ${GEN_DIR}
VOLUME  ${MAVEN_HOME}/.m2/repository

# Required from a licensing standpoint
COPY ./LICENSE ${GEN_DIR}

# Required to compile openapi-generator
COPY ./google_checkstyle.xml ${GEN_DIR}

# Modules are copied individually here to allow for caching of docker layers between major.minor versions
# NOTE: openapi-generator-online is not included here
COPY ./modules/openapi-generator-maven-plugin ${GEN_DIR}/modules/openapi-generator-maven-plugin
COPY ./modules/openapi-generator-cli ${GEN_DIR}/modules/openapi-generator-cli
COPY ./modules/openapi-generator ${GEN_DIR}/modules/openapi-generator
COPY ./pom.xml ${GEN_DIR}

# Pre-compile openapi-generator-cli
RUN mvn -am -pl "modules/openapi-generator-cli" package

# This exists at the end of the file to benefit from cached layers when modifying docker-entrypoint.sh.
COPY docker-entrypoint.sh /usr/local/bin/

ENTRYPOINT ["docker-entrypoint.sh"]

CMD ["help"]
