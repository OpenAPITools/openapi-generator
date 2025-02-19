# OpenAPI Petstore

This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\


## Overview
This project was generated using the Helidon OpenAPI Generator.

The generated classes use the programming model from the Helidon WebClient implementation, primarily the `WebClient` interface and its
`WebClient.Builder` class. Refer to the Helidon WebClient documentation for complete information about them.

## Using the Generated Classes and Interfaces
The generated `ApiClient` class wraps a `WebClient` instance. Similarly, the `ApiClient.Builder` class wraps the `WebClient.Builder` class.

The generated `xxxApi` interfaces and `xxxApiImpl` classes make it very simple for your code to send requests (with input parameters) to the remote service which the OpenAPI document describes and to process the response (with output values) from the remote service.

To use the generated API, your code performs the following steps.

1. Create an instance of the `ApiClient` using its `Builder`.
2. Create an instance of a `xxxApi` it wants to access, typically by invoking `xxxApiImpl.create(ApiClient)` and passing the `ApiClient` instance just created.
3. Invoke any of the `public` methods on the `xxxApi` instance, passing the input parameters and saving the returned `Single<WebClientResponse>` object.
4. Invoke methods on the returned `Single<WebClientResponse>` to process the response and any output from it.

Browse the methods and JavaDoc on the generated classes for more information.
