# swagger-petstore-spring-cloud

Spring cloud (Feign) client can be generated using the below command :
```shell
swagger-codegen-cli generate \
   -l spring \
   -i http://petstore.swagger.io/v2/swagger.json \
   -DhideGenerationTimestamp=true   
```
example is [here](https://github.com/swagger-api/swagger-codegen/blob/master/bin/spring-cloud-feign-petstore.sh)

## Requirements

Building the API client library requires [Maven](https://maven.apache.org/) to be installed.

## Installation

To install the API client library to your local Maven repository, simply execute:

```shell
mvn install
```

To deploy it to a remote Maven repository instead, configure the settings of the repository and execute:

```shell
mvn deploy
```

Refer to the [official documentation](https://maven.apache.org/plugins/maven-deploy-plugin/usage.html) for more information.

### Maven users

Add this dependency to your project's POM:

```xml
<dependency>
    <groupId>io.swagger</groupId>
    <artifactId>swagger-petstore-spring-cloud</artifactId>
    <version>1.0.0</version>
    <scope>compile</scope>
</dependency>
```

### Gradle users

Add this dependency to your project's build file:

```groovy
compile "io.swagger:swagger-petstore-spring-cloud:1.0.0"
```

### Others

At first generate the JAR by executing:

mvn package

Then manually install the following JARs:

* target/swagger-petstore-spring-cloud-1.0.0.jar
* target/lib/*.jar
