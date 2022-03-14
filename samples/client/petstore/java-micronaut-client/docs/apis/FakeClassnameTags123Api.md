# FakeClassnameTags123Api

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testClassname**](FakeClassnameTags123Api.md#testClassname) | **PATCH** /fake_classname_test | To test class name in snake case


## Creating FakeClassnameTags123Api

To initiate an instance of `FakeClassnameTags123Api`, you can use micronaut's `ApplicationContext`:
```java
/* imports
import io.micronaut.runtime.Micronaut;
import io.micronaut.context.ApplicationContext;
import org.openapitools.api.FakeClassnameTags123Api;
*/

ApplicationContext context = Micronaut.run(/* ... */);
PetApi apiInstance = context.getBean(FakeClassnameTags123Api.class);
```

Or the `@Inject` annotation:
```java
@Singleton
class MyClass {
    @Inject
    FakeClassnameTags123Api fakeClassnameTags123Api;

    /* ... use the injected variable */
}
```
Note that the class needs to be annotated with one of Micronaut's [scope annotations](https://docs.micronaut.io/latest/guide/#scopes) like `Singleton` in order to be processed.

More information can be found inside [Inversion of Control guide section](https://docs.micronaut.io/latest/guide/#ioc).

<a name="testClassname"></a>
# **testClassname**
```java
Mono<ModelClient> FakeClassnameTags123Api.testClassname(_body)
```

To test class name in snake case

To test class name in snake case

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_body** | [**ModelClient**](ModelClient.md)| client model |


### Return type
[**ModelClient**](ModelClient.md)

### Authorization
* **[api_key_query](auth.md#api_key_query)**

### HTTP request headers
 - **Content-Type**: `application/json`
 - **Accept**: `application/json`

