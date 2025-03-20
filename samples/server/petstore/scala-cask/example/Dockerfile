FROM virtuslab/scala-cli:latest as build
WORKDIR /app
COPY ./Server.scala /app/
# note: this assumes a published server stub jar.
# If you've published this locally, you would need to copy those into this image,
# perhaps by using coursier fetch
RUN scala-cli --power package /app/Server.scala --assembly -o app.jar

# The main image
FROM openjdk:23-slim
WORKDIR /app
COPY --from=build /app/app.jar /app/
ENTRYPOINT ["java", "-jar", "/app/app.jar"]
