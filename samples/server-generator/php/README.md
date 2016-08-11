# Swagger generated server

## Overview
Using the swagger-codegen, you can not only generate clients but servers as well!  The same spec can be used to drive your
development both ways.  This is an example of generating a server for `PHP`.

### Prerequisites
You need the following installed and available in your $PATH:

<li>- Scala 2.9.1 [available here](http://www.scala-lang.org)

You also need to add scala binary to your PATH.

You need an apache server running with mod_rewrite enabled

### Generating a server
You first need to build the `swagger-codegen` project--this is done by running this command at the root of the swagger-codegen project:

```
mvn package
```

You can now generate a server from any valid[**](https://github.com/swagger-api/swagger-codegen/blob/master/README.md#validating-your-swagger-spec) OpenAPI Spec:

```
./bin/runscala.sh samples/server-generator/php/PHPServerFromSpec.scala http://petstore.swagger.wordnik.com/api/api-docs special-key
```

After executing this script, you will have an output directory with the server-generated files:

```
$ cd samples/server-generator/php/output
$ find .  -type f
./.htaccess
./composer.json
./index.php
./README.md
```

To install the dependencies, cd to the `samples/server-generator/php/output` folder and run:

```
$ curl -s http://getcomposer.org/installer | php
$ php composer.phar install
```

You can now access the api by going to `http://localhost/path-to-output-dir/`.  Of course this isn't a fully
runnable server!  You have to add the logic in the index.php file.  But that's the easy part.


### Making it your own
Running the sample is easy, but how about making your own server?  Easy!  Just modify the `samples/server-generator/php/PHPServerGenerator.scala` file.

Don't like the templates?  Don't worry, we're not offended!  They're [mustache](http://mustache.github.com/) templates and are easy to modify.
Take a look at the sample templates here:

<li> - Generator for the index.php file : [api.mustache](https://github.com/swagger-api/swagger-codegen/blob/master/samples/server-generator/php/templates/index.mustache)

Sound easy?  It is!
