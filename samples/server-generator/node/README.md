# Swagger generated server

## Overview
Using the swagger-codegen, you can not only generate clients but servers as well!  The same spec can be used to drive your
development both ways.  This is an example of generating a server for `node.js`.

### Prerequisites
You need the following installed and available in your $PATH:

<li>- node (http://nodejs.org)

<li>- Scala 2.9.1 [available here](http://www.scala-lang.org)

You also need to add the scala binary to your PATH.

### Generating a server
You first need to build the `swagger-codegen` project--this is done by running this command at the root of the swagger-codegen project:

```
mvn package
```

You can now generate a server from any valid[**](https://github.com/wordnik/swagger-codegen/blob/master/README.md#validating-your-swagger-spec) swagger spec:

```
./bin/runscala.sh samples/server-generator/node/NodeServerFromSpec.scala http://petstore.swagger.wordnik.com/api/api-docs.json special-key
```

After executing this script, you will have an output directory with the server-generated files:

```
$ find samples/server-generator/node/output
samples/server-generator/node/output
samples/server-generator/node/output/App
samples/server-generator/node/output/App/apis
samples/server-generator/node/output/App/apis/PetApi.js
samples/server-generator/node/output/App/apis/StoreApi.js
samples/server-generator/node/output/App/apis/UserApi.js
samples/server-generator/node/output/App/Common
samples/server-generator/node/output/App/Common/node
samples/server-generator/node/output/App/Common/node/paramTypes.js
samples/server-generator/node/output/App/Common/node/randomizer.js
samples/server-generator/node/output/App/Common/node/swagger.js
samples/server-generator/node/output/App/main.js
samples/server-generator/node/output/App/models.js
samples/server-generator/node/output/package.json
```

To run the server, cd to the `samples/server-generator/node/output` folder and run:

```
# install the dependencies
npm install
node Apps/main.js
```

You can now load the swagger-ui against `http://localhost:8002/resources.json`.  Of course this isn't a fully
runnable server!  You have to add the logic in the apis/*.js files.  But that's the easy part.


### Making it your own
Running the sample is easy, but how about making your own server?  Easy!  Just modify the `samples/server-generator/node/NodeServerFromSpec.scala` file.
See comments in below, in a copy of the script

```scala
object NodeServerGenerator extends BasicScalaGenerator {
  def main(args: Array[String]) = generateClient(args)

  // if you want to point to a different template directory, change this
  override def templateDir = "samples/server-generator/node/templates"

  // where the files are written
  val outputFolder = "samples/server-generator/node/output"
    
  // where to write generated code
  override def destinationDir = outputFolder + "/App"

  // template used for apis (writes one file per api)
  apiTemplateFiles ++= Map("api.mustache" -> ".js")
  
  modelTemplateFiles.clear

  // puts the api files in a folder called `apis`
  override def apiPackage = Some("apis")

  // copies swagger files and processes any *.mustache files
  override def supportingFiles = List(
    ("package.json", outputFolder, "package.json"),
    ("README.json", outputFolder, "README.md"),
    ("main.mustache", destinationDir, "main.js"),
    ("models.mustache", destinationDir, "models.js"))
}
```

Don't like the templates?  Don't worry, we're not offended!  They're [mustache](http://mustache.github.com/) templates and are easy to modify.
Take a look at the sample templates here:

<li> Generator for your api classes: [api.mustache](https://github.com/wordnik/swagger-codegen/blob/master/samples/server-generator/node/templates/api.mustache)

<li> Generator for your models: [models.mustache](https://github.com/wordnik/swagger-codegen/blob/master/samples/server-generator/node/templates/models.mustache)

<li> The main class to run your server: [main.mustache](https://github.com/wordnik/swagger-codegen/blob/master/samples/server-generator/node/templates/main.mustache)


Sound easy?  It is!

