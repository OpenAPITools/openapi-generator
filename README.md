# Swagger Code Generator

## Overview
This is the swagger codegen project, which allows generation of client libraries automatically from a 
Swagger-compliant server.  You can find out more about both the spec and the framework at 
http://swagger.wordnik.com.  For more information about Wordnik's APIs, please visit http://developer.wordnik.com.  

### Prerequisites
You need the following installed and available in your $PATH:

* [Java 1.6](http://java.oracle.com)

* [Apache maven 3.0.3 or greater](http://maven.apache.org/)

* [Scala 2.9.1](http://www.scala-lang.org)

You also need to add the scala binary to your PATH.

After cloning the project, you need to build it from source with this command:

```
./sbt assembly
```

### To generate a sample client library
You can build a client against Wordnik's [petstore](http://petstore.swagger.wordnik.com) API as follows:

```
./bin/scala-petstore.sh
```

This will run the script in [samples/client/petstore/ScalaPetstoreCodegen.scala](https://github.com/wordnik/swagger-codegen/blob/master/samples/client/petstore/scala/ScalaPetstoreCodegen.scala) and create the client.  You can then
compile and run the client, as well as unit tests against it:

```
cd samples/client/petstore/scala
mvn package
```

Other languages have petstore samples, too:
```
./bin/flash-petstore.sh
./bin/java-petstore.sh
./bin/objc-petstore.sh
./bin/php-petstore.sh
./bin/python-petstore.sh
./bin/python3-petstore.sh
./bin/ruby-petstore.sh
```

### Generating libraries from your server
It's just as easy--you can either run the default generators:

```
./bin/runscala.sh com.wordnik.swagger.codegen.BasicScalaGenerator http://petstore.swagger.wordnik.com/api/resources.json special-key
```

Replace `Scala` with `Flash`, `Java`, `Objc`, `PHP`, `Python`, `Python3`, `Ruby`.

You will probably want to override some of the defaults--like packages, etc.  For doing this, just create a scala
script with the overrides you want.  Follow [ScalaPetstoreCodegen](https://github.com/wordnik/swagger-codegen/blob/master/samples/client/petstore/scala/ScalaPetstoreCodegen.scala) as an example:

For example, create `src/main/scala/MyCodegen.scala` with these contents:

```scala
import com.wordnik.swagger.codegen.BasicScalaGenerator

object MyCodegen extends BasicScalaGenerator {
  def main(args: Array[String]) = generateClient(args)

  // location of templates
  override def templateDir = "scala"

  // where to write generated code
  override def destinationDir = "client/scala/src/main/scala"

  // api invoker package
  override def invokerPackage = "com.myapi.client"

  // package for models
  override def modelPackage = Some("com.myapi.client.model")

  // package for api classes
  override def apiPackage = Some("com.myapi.client.api")

  // supporting classes
  override def supportingFiles = List(
    ("apiInvoker.mustache", destinationDir + java.io.File.separator + packageName.replaceAll("\\.", java.io.File.separator), "ApiInvoker.scala"),
    ("pom.mustache", destinationDir, "pom.xml")
  )
}
```

Now you can generate your client like this:

```
./bin/runscala.sh src/main/scala/MyCodegen.scala http://my.api.com/resources.json super-secret-key
```

w00t!  Thanks to the scala interpretor, you didn't even need to recompile.

### Modifying the client library format
Don't like the default swagger client syntax?  Want a different language supported?  No problem!  Swagger codegen
processes mustache templates with the [scalate](http://scalate.fusesource.org/) engine.  You can modify our templates or
make your own.

You can look at `src/main/resources/${your-language}` for examples.  To make your own templates, create your own files
and override the `templateDir` in your script to point to the right place.  It actually is that easy.

### Where is Javascript???
See our [javascript library](http://github.com/wordnik/swagger.js)--it's completely dynamic and doesn't require
static code generation.

#### Generating a client from flat files (i.e. no remote server calls)
If you don't want to call your server, you can save the swagger spec files into a directory and pass an argument
to the code generator like this:

```
-DfileMap=/path/to/files
```

Or for example:
```
./bin/java-petstore-filemap.sh
```

Which simple passes `-DfileMap=src/test/resources/petstore` as an argument.  Great for creating libraries on your
ci server... or while coding on an airplane.

### Validating your swagger spec
You can use the validation tool to see that your server is creating a proper spec file.  If you want to learn
more about the spec file and format, please see [swagger-core](https://github.com/wordnik/swagger-core/wiki).  This
tool will read the server and generate a report of any violations of the spec.  If there are violations, the
client codegen and ui may not work correctly.

To validate an api and write output to ./swagger-errors.html:

```
./bin/validate.sh http://petstore.swagger.wordnik.com/api/resources.json "" ./swagger-errors.html
```

### Generating static api documentation
If you need to make static pages or don't want the sandbox of the swagger-ui, you can use the codegen to build them.  Remember, the engine is just using mustache templates--the output format is your call.

```
./bin/static-docs.sh
```

Will produce the output here:

```
https://github.com/wordnik/swagger-codegen/tree/master/samples/docs/swagger-static-docs/sd
```

which is based on these templates:

```
https://github.com/wordnik/swagger-codegen/tree/master/src/main/resources/swagger-static
```

### To build a server stub

You can also use the codegen to generate a server for a couple different frameworks.  Take a look here:

* [javascript node.js Server generator](https://github.com/wordnik/swagger-codegen/tree/master/samples/server-generator/node)

* [ruby sinatra generator](https://github.com/wordnik/swagger-codegen/tree/master/samples/server-generator/sinatra)

* [scala scalatra generator](https://github.com/wordnik/swagger-codegen/tree/master/samples/server-generator/scalatra)

### To build the codegen library

This will create the swagger-codegen library from source.  

```
./sbt assembly
```

Note!  The templates are included in the library generated.  If you want to modify the templates, you'll need to
either repackage the library OR modify your codegen script to use a file path!

License
-------

Copyright 2012 Wordnik, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at [apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
