FROM jimschubert/8-jdk-alpine-mvn:1.0

ENV GEN_DIR /opt/swagger-codegen

RUN set -x && \
    apk add --no-cache bash

RUN mkdir /opt

ADD . ${GEN_DIR}

VOLUME  ${MAVEN_HOME}/.m2/repository

WORKDIR ${GEN_DIR}

RUN mvn -am -pl "modules/swagger-codegen-cli" package

COPY docker-entrypoint.sh /usr/local/bin/

ENTRYPOINT ["docker-entrypoint.sh"]

CMD ["build"]
