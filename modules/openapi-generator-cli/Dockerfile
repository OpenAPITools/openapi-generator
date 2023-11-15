FROM amazoncorretto:17.0.8-alpine3.18

ADD target/openapi-generator-cli.jar /opt/openapi-generator/modules/openapi-generator-cli/target/openapi-generator-cli.jar

COPY docker-entrypoint.sh /usr/local/bin/

ENTRYPOINT ["docker-entrypoint.sh"]

CMD ["help"]
