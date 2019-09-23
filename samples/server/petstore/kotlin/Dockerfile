FROM openjdk:8-jre-alpine

COPY ./build/libs/kotlin-petstore-server.jar /root/kotlin-petstore-server.jar

WORKDIR /root

CMD ["java", "-server", "-Xms4g", "-Xmx4g", "-XX:+UseG1GC", "-XX:MaxGCPauseMillis=100", "-XX:+UseStringDeduplication", "-jar", "kotlin-petstore-server.jar"]