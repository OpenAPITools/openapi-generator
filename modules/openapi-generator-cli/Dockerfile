FROM eclipse-temurin:17.0.9_9-jre-focal

ADD target/openapi-generator-cli.jar /opt/openapi-generator/modules/openapi-generator-cli/target/openapi-generator-cli.jar

COPY docker-entrypoint.sh /usr/local/bin/

RUN ln -s /usr/local/bin/docker-entrypoint.sh /usr/local/bin/openapi-generator-cli

ENTRYPOINT ["docker-entrypoint.sh"]

CMD ["help"]
