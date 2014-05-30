# Swagger generated server

## Overview
Using the swagger-codegen, you can not only generate clients but servers as well!  The same spec can be used to drive your
development both ways.  This is an example of generating a server for `JAX-RS`.

### Prerequisites
You need the following installed and available in your $PATH:

<li>Maven 3</li>

### Generating a server
You first need to build the `swagger-codegen` project--this is done by running this command at the root of the swagger-codegen project:

```
sbt assembly
```

You can now generate a server from any valid[**](https://github.com/wordnik/swagger-codegen/blob/master/README.md#validating-your-swagger-spec) swagger spec:

```
./bin/runscala.sh samples/server-generator/java-jaxrs/JavaJaxRSServerGenerator.scala http://petstore.swagger.wordnik.com/api/api-docs special-key
```

After executing this script, you will have an output directory with the server-generated files:

```
$ cd samples/server-generator/java-jaxrs/output
$ find .
./pom.xml
./README.md
./src
./src/main
./src/main/java
./src/main/java/com
./src/main/java/com/wordnik
./src/main/java/com/wordnik/api
./src/main/java/com/wordnik/api/ApiException.java
./src/main/java/com/wordnik/api/ApiResponse.java
./src/main/java/com/wordnik/api/JacksonJsonProvider.java
./src/main/java/com/wordnik/api/NotFoundException.java
./src/main/java/com/wordnik/api/PetApi.java
./src/main/java/com/wordnik/api/StoreApi.java
./src/main/java/com/wordnik/api/UserApi.java
./src/main/java/com/wordnik/client
./src/main/java/com/wordnik/client/model
./src/main/java/com/wordnik/client/model/Category.java
./src/main/java/com/wordnik/client/model/Order.java
./src/main/java/com/wordnik/client/model/Pet.java
./src/main/java/com/wordnik/client/model/Tag.java
./src/main/java/com/wordnik/client/model/User.java
./src/main/webapp
./src/main/webapp/WEB-INF
./src/main/webapp/WEB-INF/web.xml

```

To run the server, cd to the `samples/server-generator/java-jaxrs/output` folder and run:

```
mvn jetty:run
```

You can now load the swagger-ui against `http://localhost:8002/api/api-docs.json`.  Of course this isn't a fully
runnable server!  You have to add the logic in the **/api/*.java files.  But that's the easy part.

### Making it your own
Running the sample is easy, but how about making your own server?  Easy!  Just modify the `samples/server-generator/java-jaxrs/JavaJaxRSServerGenerator.scala` file.

Don't like the templates?  Don't worry, we're not offended!  They're [mustache](http://mustache.github.com/) templates and are easy to modify.
Take a look at the sample templates here:

<li> - Generator for your api classes: [api.mustache](https://github.com/wordnik/swagger-codegen/blob/master/samples/server-generator/java-jaxrs/templates/api.mustache)

<li> - Generator for your models: [model.mustache](https://github.com/wordnik/swagger-codegen/blob/master/samples/server-generator/java-jaxrs/templates/model.mustache)

<li> - Your web.xml: [web.mustache](https://github.com/wordnik/swagger-codegen/blob/master/samples/server-generator/java-jaxrs/templates/web.mustache)


Sound easy?  It is!