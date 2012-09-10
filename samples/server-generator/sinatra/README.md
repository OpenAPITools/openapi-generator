# Swagger generated server

## Overview
Using the swagger-codegen, you can not only generate clients but servers as well!  The same spec can be used to drive your
development both ways.  This is an example of generating a server for `node.js`.

### Prerequisites
You need the following installed and available in your $PATH:

<li>- Scala 2.9.1 [available here](http://www.scala-lang.org)

You also need to add scala binary to your PATH.

### Generating a server
You first need to build the `swagger-codegen` project--this is done by running this command at the root of the swagger-codegen project:

```
mvn package
```

You can now generate a server from any valid[**](https://github.com/wordnik/swagger-codegen/blob/master/README.md#validating-your-swagger-spec) swagger spec:

```
./bin/runscala.sh samples/server-generator/sinatra/SinatraServerGenerator.scala http://petstore.swagger.wordnik.com/api/resources.json special-key
```

After executing this script, you will have an output directory with the server-generated files:

```
$ cd samples/server-generator/sinatra/output
$ find .  -type f
./config.ru
./Gemfile
./Gemfile.lock
./lib/pet_api.rb
./lib/store_api.rb
./lib/swaggering.rb
./lib/user_api.rb
./my_app.rb
./README.md
```

To run the server, cd to the `samples/server-generator/sinatra/output` folder and run:

```
rackup -p 4567
```

You can now load the swagger-ui against `http://localhost:4567/resources.json`.  Of course this isn't a fully
runnable server!  You have to add the logic in the lib/*.rb files.  But that's the easy part.


### Making it your own
Running the sample is easy, but how about making your own server?  Easy!  Just modify the `samples/server-generator/sinatra/SinatraServerGenerator.scala` file.

Don't like the templates?  Don't worry, we're not offended!  They're [mustache](http://mustache.github.com/) templates and are easy to modify.
Take a look at the sample templates here:

<li> - Generator for your api classes: [api.mustache](https://github.com/wordnik/swagger-codegen/blob/master/samples/server-generator/sinatra/templates/api.mustache)

<li> - The main class to run your server: [my_app.mustache](https://github.com/wordnik/swagger-codegen/blob/master/samples/server-generator/sinatra/templates/my_app.mustache)


Sound easy?  It is!
