# swagger-petstore-okhttp-gson

## Requirements

Building the API client library requires [Maven](https://maven.apache.org/) to be installed.

## Installation

To install the API client library to your local Maven repository, simply execute:

```shell
mvn install
```

To deploy it to a remote Maven repository instead, configure the settings of the repository and execute:

```shell
mvn deploy
```

Refer to the [official documentation](https://maven.apache.org/plugins/maven-deploy-plugin/usage.html) for more information.

### Maven users

Add this dependency to your project's POM:

```xml
<dependency>
    <groupId>io.swagger</groupId>
    <artifactId>swagger-petstore-okhttp-gson</artifactId>
    <version>1.0.0</version>
    <scope>compile</scope>
</dependency>
```

### Gradle users

Add this dependency to your project's build file:

```groovy
compile "io.swagger:swagger-petstore-okhttp-gson:1.0.0"
```

### Others

At first generate the JAR by executing:

    mvn package

Then manually install the following JARs:

* target/swagger-petstore-okhttp-gson-1.0.0.jar
* target/lib/*.jar

## Getting Started

Please follow the [installation](#installation) instruction and execute the following Java code:

```java

import io.swagger.client.*;
import io.swagger.client.auth.*;
import io.swagger.client.model.*;
import io.swagger.client.api.FakeApi;

import java.io.File;
import java.util.*;

public class FakeApiExample {

    public static void main(String[] args) {
        
        FakeApi apiInstance = new FakeApi();
        String testCodeInjectEnd = "testCodeInjectEnd_example"; // String | To test code injection  ' \" =end
        try {
            apiInstance.testCodeInjectEnd(testCodeInjectEnd);
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#testCodeInjectEnd");
            e.printStackTrace();
        }
    }
}

```

## Documentation for API Endpoints

All URIs are relative to *https://petstore.swagger.io  &#39; \&quot; &#x3D;end/v2  &#39; \&quot; &#x3D;end*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*FakeApi* | [**testCodeInjectEnd**](docs/FakeApi.md#testCodeInjectEnd) | **PUT** /fake | To test code injection  &#39; \&quot; &#x3D;end


## Documentation for Models

 - [ModelReturn](docs/ModelReturn.md)


## Documentation for Authorization

Authentication schemes defined for the API:
### api_key

- **Type**: API key
- **API key parameter name**: api_key  */ &#39; &quot; &#x3D;end
- **Location**: HTTP header

### petstore_auth

- **Type**: OAuth
- **Flow**: implicit
- **Authorization URL**: http://petstore.swagger.io/api/oauth/dialog
- **Scopes**: 
  - write:pets: modify pets in your account  */ &#39; &quot; &#x3D;end
  - read:pets: read your pets  */ &#39; &quot; &#x3D;end


## Recommendation

It's recommended to create an instance of `ApiClient` per thread in a multithreaded environment to avoid any potential issue.

## Author

apiteam@swagger.io  &#39; \&quot; &#x3D;end

