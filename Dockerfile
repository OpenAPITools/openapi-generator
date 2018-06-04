FROM openjdk:8-jdk-alpine

RUN set -x && \
    apk add --no-cache bash

ENV GEN_DIR /opt/openapi-generator
ENV GRADLE_USER_HOME /opt/gradle_user_home
WORKDIR ${GEN_DIR}
VOLUME  ${GRADLE_USER_HOME}

# Required from a licensing standpoint
COPY ./LICENSE ${GEN_DIR}

# Required to compile openapi-generator
COPY ./google_checkstyle.xml ${GEN_DIR}

# Modules are copied individually here to allow for caching of docker layers between major.minor versions
# NOTE: openapi-generator-online is not included here
COPY ./modules/openapi-generator-maven-plugin ${GEN_DIR}/modules/openapi-generator-maven-plugin
COPY ./modules/openapi-generator-cli ${GEN_DIR}/modules/openapi-generator-cli
COPY ./modules/openapi-generator ${GEN_DIR}/modules/openapi-generator
COPY ./gradlew ${GEN_DIR}
COPY ./build.gradle ${GEN_DIR}
COPY ./settings.gradle ${GEN_DIR}
COPY ./gradle ${GEN_DIR}/gradle

RUN mkdir -p ${GRADLE_USER_HOME}

# Pre-compile openapi-generator-cli
RUN sh gradlew :openapi-generator-cli:assemble

# This exists at the end of the file to benefit from cached layers when modifying docker-entrypoint.sh.
COPY docker-entrypoint.sh /usr/local/bin/

ENTRYPOINT ["docker-entrypoint.sh"]

CMD ["help"]
