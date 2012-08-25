# Swagger Client Code-Generator

## Overview
This is a project to build the Swagger code-gen library which can be used to automatically
generate client libraries from a Swagger-compliant server.  You can find out more about both 
the spec and the framework at http://swagger.wordnik.com.  For more information about Wordnik's 
APIs, please visit http://developer.wordnik.com.  

### Prerequisites
You need the following installed and available in your $PATH:

<li>- Java 1.6 or greater (http://java.oracle.com)

<li>- Apache maven 3.0.3 or greater (http://maven.apache.org/)

<li>- Scala 2.9.1 [available here](http://www.scala-lang.org)

You also need to add the scala binary to your PATH.

### To generate a sample client library
You can build a client against Wordnik's [petstore](http://petstore.swagger.wordnik.com) API as follows:

```
./bin/scala-petstore.sh
```

This will run the script in `src/main/scala/ScalaPetstoreCodegen.scala` and create the client.  You can then
compile and run the client, as well as unit tests against it:

```
cd samples/petstore/scala
mvn package
```

Do the same for `java` by running `./bin/java-petstore.sh`


#### Generating a client from flat files
If you don't want to call your server, you can save the swagger spec files into a directory and pass an argument
to the code generator like this:

```
-DfileMap=/path/to/files
```

Or for example:
```
./bin/java-petstore-filemap.sh
```

Which simple passes `-DfileMap=src/test/resources/petstore` as an argument 


### Validating your swagger spec
You can use the validation tool to see that your server is creating a proper spec file.  If you want to learn
more about the spec file and format, please see [swagger-core](https://github.com/wordnik/swagger-core/wiki).  This
tool will read the server and generate a report of any violations of the spec.  If there are violations, the
client codegen and ui may not work correctly.

To validate an api and write output to ./swagger-errors.html:

```
./bin/validate.sh http://petstore.swagger.wordnik.com/api/resources.json "" ./swagger-errors.html
```

### To build the codegen library

This will create the swagger-codegen library from source.  

```
mvn package
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
