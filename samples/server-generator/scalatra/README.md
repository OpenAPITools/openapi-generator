# Swagger generated server

## Overview
Using the swagger-codegen, you can not only generate clients but servers as well!  The same spec can be used to drive your
development both ways.  This is an example of generating a server for `node.js`.

### Prerequisites
You need the following installed and available in your $PATH:

<li>- sbt 0.12 (http://www.scala-sbt.org/)

<li>- Scala 2.9.1 [available here](http://www.scala-lang.org)

You also need to add both the sbt and scala binary to your PATH.

### Generating a server
You first need to build the `swagger-codegen` project--this is done by running this command at the root of the swagger-codegen project:

```
mvn package
```

You can now generate a server from any valid[**](https://github.com/wordnik/swagger-codegen/blob/master/README.md#validating-your-swagger-spec) swagger spec:

```
./bin/runscala.sh samples/server-generator/scalatra/ScalatraServerGenerator.scala http://petstore.swagger.wordnik.com/api/resources.json special-key
```

After executing this script, you will have an output directory with the server-generated files:

```
$ cd samples/server-generator/scalatra/output
$ find  -type f
./project/build.properties
./project/plugins.sbt
./README.md
./src/main/scala/apis/PetApi.scala
./src/main/scala/apis/StoreApi.scala
./src/main/scala/apis/UserApi.scala
./src/main/scala/com/wordnik/client/model/Category.scala
./src/main/scala/com/wordnik/client/model/Order.scala
./src/main/scala/com/wordnik/client/model/Pet.scala
./src/main/scala/com/wordnik/client/model/Tag.scala
./src/main/scala/com/wordnik/client/model/User.scala
./src/main/scala/JsonUtil.scala
./src/main/scala/ServletApp.scala

```

To run the server, cd to the `samples/server-generator/scalatra/output` folder and run:

```
sbt run
```

You can now load the swagger-ui against `http://localhost:8002/api/resources.json`.  Of course this isn't a fully
runnable server!  You have to add the logic in the apis/*.scala files.  But that's the easy part.


### Making it your own
Running the sample is easy, but how about making your own server?  Easy!  Just modify the `samples/server-generator/scalatra/ScalatraServerGenerator.scala` file.

Don't like the templates?  Don't worry, we're not offended!  They're [mustache](http://mustache.github.com/) templates and are easy to modify.
Take a look at the sample templates here:

<li> - Generator for your api classes: [api.mustache](https://github.com/wordnik/swagger-codegen/blob/master/samples/server-generator/scalatra/templates/api.mustache)

<li> - Generator for your models: [model.mustache](https://github.com/wordnik/swagger-codegen/blob/master/samples/server-generator/scalatra/templates/model.mustache)

<li> - The main class to run your server: [ServletApp.mustache](https://github.com/wordnik/swagger-codegen/blob/master/samples/server-generator/scalatra/templates/ServletApp.mustache)


Sound easy?  It is!

