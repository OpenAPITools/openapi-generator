# org.openapitools.server - Kotlin Server library for Email api

## Requires

* Kotlin 1.4.31
* Gradle 6.8.2

## Build

First, create the gradle wrapper script:

```
gradle wrapper
```

Then, run:

```
./gradlew check assemble
```

This runs all tests and packages the library.

## Features/Implementation Notes

* Supports JSON inputs/outputs, File inputs, and Form inputs.
* Supports collection formats for query parameters: csv, tsv, ssv, pipes.
* Some Kotlin and Java types are fully qualified to avoid conflicts with types defined in OpenAPI definitions.

<a id="documentation-for-api-endpoints"></a>
## Documentation for API Endpoints

All URIs are relative to *https://email.mydomain.com*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*EmailApi* | [**getEmails**](docs/EmailApi.md#getemails) | **GET** /emails/{from} | Get emails sent from the given address
*EmailApi* | [**sendEmail**](docs/EmailApi.md#sendemail) | **POST** /email | Send an email


<a id="documentation-for-models"></a>
## Documentation for Models

 - [org.openapitools.server.models.EmailDefinition](docs/EmailDefinition.md)


<a id="documentation-for-authorization"></a>
## Documentation for Authorization

Endpoints do not require authorization.

