# OpenAPI for Sinatra

## Overview
This is a project to provide Swagger support inside the [Sinatra](http://www.sinatrarb.com/) framework.  You can find
out more about both the spec and the framework at http://swagger.io.  For more information about 
Wordnik's APIs, please visit http://developer.wordnik.com.

## Prerequisites
As of ruby 3.0.0, the webrick web server library was removed.
You need to install a rack-supported web server, such as webrick and thin.

The default Gemfile is as follows. 
Update the name of the web server as your prefer.

```
source 'https://rubygems.org'

gem "webrick"
gem "sinatra"
gem "sinatra-cross_origin"
```

## Getting started
To generate a ruby-sinatra server for petstore.yaml, please run the following:

```
openapi-generator-cli generate \
  -i https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml \
  -g ruby-sinatra -o code
```

To run the generated server, please run the following:

```
cd code/
bundle config set path lib
bundle install 
bundle exec rackup -p 8080
```

You can access the application by the following URL:

```
http://localhost:8080/v2/store/inventory
```

## Docker
If you want to use a web server other than webrick, you need to edit the generated Dockerfile to prepare the compiler and the make command. Please check the comment of the Dockerfile.

To run the code on docker, you can use the Dockerfile as follows:

### Build the docker image
The "container_name" can be changed for your preferences.

```
docker build . --tag "container_name"
```

### Run the docker image

```
docker run -it --rm -p 8080:8080 "container_name"
```

Voila!
