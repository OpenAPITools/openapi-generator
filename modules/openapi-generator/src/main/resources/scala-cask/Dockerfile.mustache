FROM virtuslab/scala-cli:latest as build
WORKDIR /app
COPY ./Server.scala /app/
RUN scala-cli package /app/Server.scala --assembly -o app.jar

# The main image
FROM openjdk:23-slim
WORKDIR /app
COPY --from=build /app/app.jar /app/
ENTRYPOINT ["java", "-jar", "/app/app.jar"]
