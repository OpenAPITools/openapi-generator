# org.openapitools.client - Kotlin client library for MultiResponseServer

## Requires

* Kotlin 1.3.41
* Gradle 4.9

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
* Implementation of ApiClient is intended to reduce method counts, specifically to benefit Android targets.

<a name="documentation-for-api-endpoints"></a>
## Documentation for API Endpoints

All URIs are relative to *http://localhost*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*DefaultApi* | [**multiStatusReturnGet**](docs/DefaultApi.md#multistatusreturnget) | **GET** /multiStatusReturn | Is able to return a lot of status codes


<a name="documentation-for-models"></a>
## Documentation for Models

 - [org.openapitools.client.models.FirstModel](docs/FirstModel.md)
 - [org.openapitools.client.models.FourthModel](docs/FourthModel.md)
 - [org.openapitools.client.models.SecondModel](docs/SecondModel.md)
 - [org.openapitools.client.models.ThirdModel](docs/ThirdModel.md)


<a name="documentation-for-authorization"></a>
## Documentation for Authorization

All endpoints do not require authorization.
