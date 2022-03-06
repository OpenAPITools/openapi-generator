# AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**call123testSpecialTags**](AnotherFakeApi.md#call123testSpecialTags) | **PATCH** /another-fake/dummy | To test special tags


## Creating AnotherFakeApi

To initiate an instance of `AnotherFakeApi`, you can use micronaut's `ApplicationContext`:
```java
/* imports
import io.micronaut.runtime.Micronaut;
import io.micronaut.context.ApplicationContext;
import org.openapitools.api.AnotherFakeApi;
*/

ApplicationContext context = Micronaut.run(/* ... */);
PetApi apiInstance = context.getBean(AnotherFakeApi.class);
```

Or the `@Inject` annotation:
```java
@Singleton
class MyClass {
    @Inject
    AnotherFakeApi anotherFakeApi;

    /* ... use the injected variable */
}
```
Note that the class needs to be annotated with one of Micronaut's [scope annotations](https://docs.micronaut.io/latest/guide/#scopes) like `Singleton` in order to be processed.

More information can be found inside [Inversion of Control guide section](https://docs.micronaut.io/latest/guide/#ioc).

<a name="call123testSpecialTags"></a>
# **call123testSpecialTags**
```java
Mono<ModelClient> AnotherFakeApi.call123testSpecialTags(_body)
```

To test special tags

To test special tags and operation ID starting with number

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_body** | [**ModelClient**](ModelClient.md)| client model |


### Return type
[**ModelClient**](ModelClient.md)



### HTTP request headers
 - **Content-Type**: `application/json`
 - **Accept**: `application/json`

