---
id: faq-generators
title: "FAQ: Generators"
---

### What are some server generator use cases?

We have around 40+ server generators, with more added regularly. Some of these include Spring in your choice of Java or Kotlin, the Finch and Scalatra frameworks using Scala, and C# generators for NancyFX and WebAPI (to name only a few).

Besides generating the server code as a starting point to implement the API backend, here are some use cases of the server generators:

* **prototyping** - one can generate the server code and have a functional API backend very quickly to try different things or features.
* **mocking** - easily provide an API backend for mocking based on the examples field defined in the response object.
* **migration** - let's say one wants to migrate an API backend from Ruby on Rails to Java Spring. The server generator can save a lot of time in implementing and verify each endpoint in the new API backend.
* **evaluating** - when you want to try a new language or framework, and a typical "Hello, World" is too trivial.

## Java

### The API client has SSL errors due to an invalid certificate. Is there a way to bypass that?

Yes, please refer to http://stackoverflow.com/a/6055903/677735

### How can I customize the Feign client templates?

You will need to provide customized files in `Java/libraries/feign` under the resources folder and pass the location via the `-t` option.

In your Gradle build script, please add the following (example):
```
config.templateDir = 'src/openapi-generator-templates/Java/libraries/feign
```

## Android

### How can I generate an Android SDK?

**The Java SDK is also compatible with Android.**

[RECOMMENDED] To generate the Java SDK with `okhttp` and `gson` libraries, run the following:
```
mvn clean package
java -jar modules/openapi-generator-cli/target/openapi-generator-cli.jar generate \
  -i https://raw.githubusercontent.com/OpenAPITools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.json \
  -l java --library=okhttp-gson \
  --additional-properties hideGenerationTimestamp=true \
  -o /var/tmp/java/okhttp-gson/ 
```

You can also generate the Java SDK with other HTTP libraries by replacing `okhttp-gson` with `retrofit` for example. For a list of support libraries, please run

```
java -jar modules/openapi-generator-cli/target/openapi-generator-cli.jar config-help -l java
```  

To generate the Android SDK with [`volley`](https://github.com/mcxiaoke/android-volley), please run
```
mvn clean package
java -jar modules/openapi-generator-cli/target/openapi-generator-cli.jar generate \
  -i https://raw.githubusercontent.com/OpenAPITools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.json \
  -l android --library=volley \
  -o /var/tmp/android/volley/ 
```
We do **not** recommend using the default HTTP library (Apache HttpClient) with `android` as it's not actively maintained.

## C-Sharp

### How do I fix `CSC:  warning CS2002` in Xamarin?

The full warning might look like this: `CSC:  warning CS2002: Source file 'Api/FakeApi.cs' specified multiple times`

The warning has no impact on the build process so you should be able to build the solution without issue. The warning should be addressed in the upcoming stable release of Xamarin. 

## Objective-C

### How do I run integration test with Petstore ObjC API client?

Here are the steps:
```
git clone https://github.com/openapitools/openapi-generator.git
cd openapi-generator/samples/client/petstore/objc/default/OpenAPIClientTests
mvn integration-test
```

Besides `default` (folder) ObjC API client, there's also `core-data` for another ObjC API client with [Core Data support](https://en.wikipedia.org/wiki/Core_Data).

## Swift

### How do I run integration test with Petstore Swift API client?

Here are the steps:
```
git clone https://github.com/openapitools/openapi-generator.git
cd openapi-generator/samples/client/petstore/swift/default/OpenAPIClientTests
mvn integration-test
```
Besides `default` (folder), there's another folder `promisekit` for Swift API client with [PromiseKit support](https://github.com/mxcl/PromiseKit)
```
git clone https://github.com/openapitools/openapi-generator.git
cd openapi-generator/samples/client/petstore/swift/promisekit/OpenAPIClientTests
mvn integration-test
```

### Is Swift (2.x) generator still actively maintained?

No, please use `swift3` or `swift4` generator instead as we want to focus on Swift 3.x, 4.x.

## TypeScript

### The JSON response fails to deserialize due to change in variable naming (snake_case to camelCase). Is there any way to keep the original naming?

Yes, please use the following option when generating TypeScript clients:

```
	modelPropertyNaming
	    Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name (Default: camelCase)
```
