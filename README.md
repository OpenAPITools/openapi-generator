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

### To build the codegen library

This will create the swagger-codegen library in your build folder.  

<pre>
mvn package
</pre>


### To generate a sample client library
You can build a client against Wordnik's [petstore](http://petstore.swagger.wordnik.com) API as follows:

<pre>
./bin/scala-petstore.sh
</pre>

This will run the script in `src/main/scala/ScalaPetstoreCodegen.scala` and create the client.  You can then
compile and run the client, as well as unit tests against it:

<pre>
cd samples/petstore/scala

mvn package
</pre>

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
