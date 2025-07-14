# Gradle users:
./gradlew openapiGenerate \
  -DgeneratorName=java \
  -DinputSpec=path/to/your-api.yaml \
  -DoutputDir=generated-code/java

# Maven users:
mvn openapi-generator:generate \
  -DgeneratorName=java \
  -DinputSpec=path/to/your-api.yaml \
  -DoutputDir=generated-code/java
