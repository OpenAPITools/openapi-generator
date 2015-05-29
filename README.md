# Swagger Code Generator

[![Build Status](https://travis-ci.org/swagger-api/swagger-codegen.png)](https://travis-ci.org/swagger-api/swagger-codegen)

## Overview
This is the swagger codegen project, which allows generation of client libraries automatically from a Swagger-compliant server.  

Check out [Swagger-Spec](https://github.com/swagger-api/swagger-spec) for additional information about the Swagger project, including additional libraries with support for other languages and more. 


## Compatibility
The Swagger Specification has undergone 3 revisions since initial creation in 2010.  The swagger-codegen project has the following compatibilies with the swagger specification:

Swagger Codegen Version | Release Date | Swagger Spec compatibility | Notes
----------------------- | ------------ | -------------------------- | -----
2.1.0-M2                | 2015-04-06   | 1.0, 1.1, 1.2, 2.0   | [master](https://github.com/swagger-api/swagger-codegen)
2.0.17                  | 2014-08-22   | 1.1, 1.2      | [tag v2.0.17](https://github.com/swagger-api/swagger-codegen/tree/2.0.17)
1.0.4                   | 2012-04-12   | 1.0, 1.1      | [tag v1.0.4](https://github.com/swagger-api/swagger-codegen/tree/swagger-codegen_2.9.1-1.1)


### Prerequisites
You need the following installed and available in your $PATH:

* [Java 7](http://java.oracle.com)

* [Apache maven 3.0.3 or greater](http://maven.apache.org/)
 
#### OS X Users
Don't forget to install Java 7. You probably have 1.6 or 1.8.

Export JAVA_HOME in order to user proper Java version:
```
export JAVA_HOME=`/usr/libexec/java_home -v 1.7`
export PATH=${JAVA_HOME}/bin:$PATH
```

#### Building

After cloning the project, you can build it from source with this command:

```
mvn package
```

### To generate a sample client library
You can build a client against the swagger sample [petstore](http://petstore.swagger.io) API as follows:

```
./bin/java-petstore.sh
```

This will run the generator with this command:

```
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l java \
  -o samples/client/petstore/java
```

With a number of options.  You can get the options with the `help generate` command:

```
NAME
        swagger generate - Generate code with chosen lang

SYNOPSIS
        swagger generate [(-a <authorization> | --auth <authorization>)]
                (-i <spec file> | --input-spec <spec file>)
                (-l <language> | --lang <language>)
                [(-o <output directory> | --output <output directory>)]
                [(-t <template directory> | --template-dir <template directory>)]
                [(-v | --verbose)]

OPTIONS
        -a <authorization>, --auth <authorization>
            adds authorization headers when fetching the swagger definitions
            remotely. Pass in a URL-encoded string of name:header with a comma
            separating multiple values

        -i <spec file>, --input-spec <spec file>
            location of the swagger spec, as URL or file (required)

        -l <language>, --lang <language>
            client language to generate (maybe class name in classpath,
            required)

        -o <output directory>, --output <output directory>
            where to write the generated files (current dir by default)

        -t <template directory>, --template-dir <template directory>
            folder containing the template files

        -v, --verbose
            verbose mode
 ```

You can then compile and run the client, as well as unit tests against it:

```
cd samples/client/petstore/java
mvn package
```

Other languages have petstore samples, too:
```
./bin/android-petstore.sh
./bin/java-petstore.sh
./bin/objc-petstore.sh
```

### Generating libraries from your server
It's just as easy--just use the `-i` flag to point to either a server or file.

### Modifying the client library format
Don't like the default swagger client syntax?  Want a different language supported?  No problem!  Swagger codegen processes mustache templates with the [jmustache](https://github.com/samskivert/jmustache) engine.  You can modify our templates or make your own.

You can look at `modules/swagger-codegen/src/main/resources/${your-language}` for examples.  To make your own templates, create your own files and use the `-t` flag to specify your template folder.  It actually is that easy.

### Making your own codegen modules
If you're starting a project with a new language and don't see what you need, swagger-codegen can help you create a project to generate your own libraries:

```
java -jar modules/swagger-codegen-distribution/target/swagger-codegen-cli.jar meta \
  -o output/myLibrary -n myClientCodegen -p com.my.company.codegen
```

This will write, in the folder `output/myLibrary`, all the files you need to get started, including a README.md. Once modified and compiled, you can load your library with the codegen and generate clients with your own, custom-rolled logic.

### Where is Javascript???
See our [javascript library](http://github.com/swagger-api/swagger-js)--it's completely dynamic and doesn't require
static code generation.
There is a third-party component called [swagger-js-codegen](https://github.com/wcandillon/swagger-js-codegen) that can generate angularjs or nodejs source code from a swagger specification.


#### Generating a client from flat files (i.e. no remote server calls)
If you don't want to call your server, you can save the swagger spec files into a directory and pass an argument
to the code generator like this:

```
-i ./modules/swagger-codegen/src/test/resources/2_0/petstore.json
```

Great for creating libraries on your ci server, from the [Swagger Editor](http://editor.swagger.io)... or while coding on an airplane.

### Customizing the generator

There are different aspects of customizing the code generator beyond just creating or modifying templates.  Each language has a supporting configuration file to handle different type mappings, etc:

```
$ ls -1 modules/swagger-codegen/src/main/java/com/wordnik/swagger/codegen/languages/
AndroidClientCodegen.java
AsyncScalaClientCodegen.java
CSharpClientCodegen.java
JavaClientCodegen.java
JaxRSServerCodegen.java
NodeJSServerCodegen.java
ObjcClientCodegen.java
PhpClientCodegen.java
PythonClientCodegen.java
RubyClientCodegen.java
ScalaClientCodegen.java
ScalatraServerCodegen.java
SpringMVCServerCodegen.java
StaticDocCodegen.java
StaticHtmlGenerator.java
SwaggerGenerator.java
SwaggerYamlGenerator.java
TizenClientCodegen.java
```

Each of these files creates reasonable defaults so you can get running quickly.  But if you want to configure package names, prefixes, model folders, etc., you may want to extend these.

To change, for example, the prefix for the Objective-C generated files, simply subclass the ObjcClientCodegen.java:

```
package com.mycompany.swagger.codegen;

import com.wordnik.swagger.codegen.languages.*;

public class MyObjcCodegen extends ObjcClientCodegen {
  static {
    PREFIX = "HELO";
  }
}
```

and specify the `classname` when running the generator:

```
-l com.mycompany.swagger.codegen.MyObjcCodegen
```

Your subclass will now be loaded and overrides the `PREFIX` value in the superclass.

### Validating your swagger spec

You have options.  The easiest is to use our [online validator](https://github.com/swagger-api/validator-badge) which not only will let you validate your spec, but with the debug flag, you can see what's wrong with your spec.  For example:

http://online.swagger.io/validator/debug?url=http://petstore.swagger.io/v2/swagger.json

### Generating dynamic html api documentation

To do so, just use the `-l dynamic-html` flag when reading a spec file.  This creates HTML documentation that is available as a single-page application with AJAX.  To view the documentation:

```
cd samples/dynamic-html/
npm install
node .
```

Which launches a node.js server so the AJAX calls have a place to go.


### Generating static html api documentation

To do so, just use the `-l html` flag when reading a spec file.  This creates a single, simple HTML file with embedded css so you can ship it as an email attachment, or load it from your filesystem:

```
cd samples/html/
open index.html
```


### To build a server stub

You can also use the codegen to generate a server for a couple different frameworks.  Take a look here:

### node.js
```
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l nodejs \
  -o samples/server/petstore/nodejs
```

### rails-grape
#### Not yet migrated to this branch


### scala scalatra
```
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l scalatra \
  -o samples/server/petstore/scalatra
```

### java jax-rs

```
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l jaxrs \
  -o samples/server/petstore/jaxrs
```

### java spring-mvc

```
java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
  -i http://petstore.swagger.io/v2/swagger.json \
  -l spring-mvc \
  -o samples/server/petstore/spring-mvc
```
### To build the codegen library

This will create the swagger-codegen library from source.  

```
mvn package
```

Note!  The templates are included in the library generated.  If you want to modify the templates, you'll need to either repackage the library OR specify a path to your scripts

License
-------

Copyright 2015 SmartBear Software

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at [apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
