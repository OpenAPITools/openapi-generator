# Wordnik Objective-C client library

## Overview
This is a full client library for the Wordnik API.  It requires that you have a valid Wordnik API Key--you
can get one for free at http://developer.wordnik.com.

This library is built using the Wordnik [Swagger](http://swagger.wordnik.com) client library generator.  You
can re-generate this library by running ./bin/objc-wordnik-api.sh from the swagger-codegen project

## Usage
There is a simple hello world example here: (https://github.com/wordnik/swagger-codegen/blob/master/samples/client/wordnik-api/objc/WordnikApiClient/WordnikApiClient/main.m)

It is recommended that you review the OCUnit tests to see how to use the library.  They live under the 
[Tests](https://github.com/wordnik/swagger-codegen/tree/master/samples/client/wordnik-api/objc/tests) directory and require that you enter your API_KEY, as well as a username
and password to authenticate with.  To do this:

* Choose "Product/Manage Schemes"

* Highlight "WordnikApiTests", click "Edit"

* Select "Test" on the left panel and add (or update the placeholder) environment variable for API_KEY, USER_NAME, PASSWORD

* Dismiss and run the tests


## Requirements
You need a valid Wordnik API key to access the API--to get one, go to [Wordnik Developer](http://developer.wordnik.com) to sign up.  It costs nothing!

This project was built with the following minimum requirements:

* iOS 5 or greater
* ARC enabled compiler