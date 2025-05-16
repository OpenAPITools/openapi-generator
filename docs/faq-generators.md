---
id: faq-generators
title: "FAQ: Generators"
---

### What are some server generator use cases?

We have around 40+ server generators, with more added regularly. Some of these include Spring in your choice of Java or Kotlin, the Finch and Scalatra frameworks using Scala, and C# generators for ASP.NET and Azure Functions (to name only a few).

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

### Which Swift generator is still actively maintained?

Please use `swift5` generator because Swift 4.x is deprecated.
There is a new `swift6` generator, that is currently in beta, try it and give us your feedback.

### How do I implement bearer token authentication with URLSession on the Swift 5 API client?

First you subclass RequestBuilderFactory

```
    class BearerRequestBuilderFactory: RequestBuilderFactory {
        func getNonDecodableBuilder<T>() -> RequestBuilder<T>.Type {
            BearerRequestBuilder<T>.self
        }

        func getBuilder<T: Decodable>() -> RequestBuilder<T>.Type {
            BearerDecodableRequestBuilder<T>.self
        }
    }
```

Then you subclass URLSessionRequestBuilder and URLSessionDecodableRequestBuilder
```
    class BearerRequestBuilder<T>: URLSessionRequestBuilder<T> {
        @discardableResult
        override func execute(_ apiResponseQueue: DispatchQueue = PetstoreClientAPI.apiResponseQueue, _ completion: @escaping (Result<Response<T>, ErrorResponse>) -> Void) -> RequestTask {
    
            // Before making the request, we can validate if we have a bearer token to be able to make a request
            BearerTokenHandler.refreshTokenIfDoesntExist {
                
                // Here we make the request
                super.execute(apiResponseQueue) { result in
                    
                    switch result {
                    case .success:
                        // If we got a successful response, we send the response to the completion block
                        completion(result)
                        
                    case let .failure(error):
                        
                        // If we got a failure response, we will analyse the error to see what we should do with it
                        if case let ErrorResponse.error(_, data, response, error) = error {
                            
                            // If the error is an ErrorResponse.error() we will analyse it to see if it's a 401, and if it's a 401, we will refresh the token and retry the request
                            BearerTokenHandler.refreshTokenIfUnauthorizedRequestResponse(
                                data: data,
                                response: response,
                                error: error
                            ) { wasTokenRefreshed in
                                
                                if wasTokenRefreshed {
                                    // If the token was refreshed, it's because it was a 401 error, so we refreshed the token, and we are going to retry the request by calling self.execute()
                                    self.execute(apiResponseQueue, completion)
                                } else {
                                    // If the token was not refreshed, it's because it was not a 401 error, so we send the response to the completion block
                                    completion(result)
                                }
                            }
                        } else {
                            // If it's an unknown error, we send the response to the completion block
                            completion(result)
                        }
                        
                    }
                }
            }
            
            return requestTask
        }
    }
    
    class BearerDecodableRequestBuilder<T: Decodable>: URLSessionDecodableRequestBuilder<T> {
        @discardableResult
        override func execute(_ apiResponseQueue: DispatchQueue = PetstoreClientAPI.apiResponseQueue, _ completion: @escaping (Result<Response<T>, ErrorResponse>) -> Void) -> RequestTask {
            // Before making the request, we can validate if we have a bearer token to be able to make a request
            BearerTokenHandler.refreshTokenIfDoesntExist {
                
                // Here we make the request
                super.execute(apiResponseQueue) { result in
                    
                    switch result {
                    case .success:
                        // If we got a successful response, we send the response to the completion block
                        completion(result)
                        
                    case let .failure(error):
                        
                        // If we got a failure response, we will analyse the error to see what we should do with it
                        if case let ErrorResponse.error(_, data, response, error) = error {
                            
                            // If the error is an ErrorResponse.error() we will analyse it to see if it's a 401, and if it's a 401, we will refresh the token and retry the request
                            BearerTokenHandler.refreshTokenIfUnauthorizedRequestResponse(
                                data: data,
                                response: response,
                                error: error
                            ) { wasTokenRefreshed in
                                
                                if wasTokenRefreshed {
                                    // If the token was refreshed, it's because it was a 401 error, so we refreshed the token, and we are going to retry the request by calling self.execute()
                                    self.execute(apiResponseQueue, completion)
                                } else {
                                    // If the token was not refreshed, it's because it was not a 401 error, so we send the response to the completion block
                                    completion(result)
                                }
                            }
                        } else {
                            // If it's an unknown error, we send the response to the completion block
                            completion(result)
                        }
                        
                    }
                }
            }
            
            return requestTask
        }
    }
    
    class BearerTokenHandler {
        private static var bearerToken: String? = nil
        
        static func refreshTokenIfDoesntExist(completionHandler: @escaping () -> Void) {
            if bearerToken != nil {
                completionHandler()
            } else {
                startRefreshingToken {
                    completionHandler()
                }
            }
        }
        
        static func refreshTokenIfUnauthorizedRequestResponse(data: Data?, response: URLResponse?, error: Error?, completionHandler: @escaping (Bool) -> Void) {
            if let response = response as? HTTPURLResponse, response.statusCode == 401 {
                startRefreshingToken {
                    completionHandler(true)
                }
            } else {
                completionHandler(false)
            }
        }
        
        private static func startRefreshingToken(completionHandler: @escaping () -> Void) {
            // Get a bearer token
            let dummyBearerToken = "..."
            
            bearerToken = dummyBearerToken
            PetstoreClientAPI.customHeaders["Authorization"] = "Bearer \(dummyBearerToken)"
    
            completionHandler()
        }
    }
```

Then you assign the `BearerRequestBuilderFactory` to the property `requestBuilderFactory`.

`PetstoreClientAPI.requestBuilderFactory = BearerRequestBuilderFactory()`

The name `PetstoreClientAPI.requestBuilderFactory` will change depending on your project name.

Here is a working sample that put's together all of this.
[AppDelegate.swift](https://github.com/OpenAPITools/openapi-generator/blob/master/samples/client/petstore/swift5/urlsessionLibrary/SwaggerClientTests/SwaggerClient/AppDelegate.swift)
[BearerDecodableRequestBuilder.swift](https://github.com/OpenAPITools/openapi-generator/blob/master/samples/client/petstore/swift5/urlsessionLibrary/SwaggerClientTests/SwaggerClient/BearerDecodableRequestBuilder.swift)

### How do I implement bearer token authentication with Alamofire on the Swift 5 API client?

First you subclass RequestBuilderFactory

```
    class BearerRequestBuilderFactory: RequestBuilderFactory {
        func getNonDecodableBuilder<T>() -> RequestBuilder<T>.Type {
            BearerRequestBuilder<T>.self
        }
        
        func getBuilder<T: Decodable>() -> RequestBuilder<T>.Type {
            BearerDecodableRequestBuilder<T>.self
        }
    }
```

Then you subclass AlamofireRequestBuilder and AlamofireDecodableRequestBuilder
```
    class BearerRequestBuilder<T>: AlamofireRequestBuilder<T> {
        override func createSessionManager() -> SessionManager {
            let sessionManager = super.createSessionManager()
            
            let bearerTokenHandler = BearerTokenHandler()
            sessionManager.adapter = bearerTokenHandler
            sessionManager.retrier = bearerTokenHandler
            
            return sessionManager
        }
    }

    class BearerDecodableRequestBuilder<T: Decodable>: AlamofireDecodableRequestBuilder<T> {
        override func createSessionManager() -> SessionManager {
            let sessionManager = super.createSessionManager()
            
            let bearerTokenHandler = BearerTokenHandler()
            sessionManager.adapter = bearerTokenHandler
            sessionManager.retrier = bearerTokenHandler
            
            return sessionManager
        }
    }

    class BearerTokenHandler: RequestAdapter, RequestRetrier {
        private static var bearerToken: String? = nil
        
        func adapt(_ urlRequest: URLRequest) throws -> URLRequest {
            if let bearerToken = Self.bearerToken {
                var urlRequest = urlRequest
                urlRequest.setValue("Bearer \(bearerToken)", forHTTPHeaderField: "Authorization")
                return urlRequest
            }
            
            return urlRequest
        }
        
        func should(_: SessionManager, retry request: Request, with _: Error, completion: @escaping RequestRetryCompletion) {
            if let response = request.task?.response as? HTTPURLResponse, response.statusCode == 401 {
                Self.startRefreshingToken { isTokenRefreshed in
                    completion(isTokenRefreshed, 0.0)
                }
            } else {
                completion(false, 0.0)
            }
        }
        
        private static func startRefreshingToken(completionHandler: @escaping (Bool) -> Void) {
            // Get a bearer token
            let dummyBearerToken = "..."
            
            bearerToken = dummyBearerToken
            PetstoreClientAPI.customHeaders["Authorization"] = "Bearer \(dummyBearerToken)"
            
            completionHandler(true)
        }
    }
```

Then you assign the `BearerRequestBuilderFactory` to the property `requestBuilderFactory`.

`PetstoreClientAPI.requestBuilderFactory = BearerRequestBuilderFactory()`

The name `PetstoreClientAPI.requestBuilderFactory` will change depending on your project name.

Here is a working sample that put's together all of this.
[AppDelegate.swift](https://github.com/OpenAPITools/openapi-generator/blob/master/samples/client/petstore/swift5/alamofireLibrary/SwaggerClientTests/SwaggerClient/AppDelegate.swift)
[BearerTokenHandler.swift](https://github.com/OpenAPITools/openapi-generator/blob/master/samples/client/petstore/swift5/alamofireLibrary/SwaggerClientTests/SwaggerClient/BearerDecodableRequestBuilder.swift)

### How do I implement bearer token authentication with URLSession on the Swift 6 API client?

First you implement the `OpenAPIInterceptor` protocol.
```
public class BearerOpenAPIInterceptor: OpenAPIInterceptor {
    public init() {}
    
    public func intercept<T>(urlRequest: URLRequest, urlSession: URLSessionProtocol, requestBuilder: RequestBuilder<T>, completion: @escaping (Result<URLRequest, any Error>) -> Void) {

        guard requestBuilder.requiresAuthentication else {
            // no authentication required
            completion(.success(urlRequest))
            return
        }

        refreshTokenIfDoesntExist { token in
            
            // Change the current url request
            var newUrlRequest = urlRequest
            newUrlRequest.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization")
            
            // Change the global headers
            requestBuilder.openAPIClient.customHeaders["Authorization"] = "Bearer \(token)"
            
            completion(.success(newUrlRequest))
        }
    }
    
    public func retry<T>(urlRequest: URLRequest, urlSession: URLSessionProtocol, requestBuilder: RequestBuilder<T>, data: Data?, response: URLResponse?, error: Error, completion: @escaping (OpenAPIInterceptorRetry) -> Void) {
        // We will analyse the response to see if it's a 401, and if it's a 401, we will refresh the token and retry the request
        refreshTokenIfUnauthorizedRequestResponse(
            data: data,
            response: response,
            error: error
        ) { (wasTokenRefreshed, newToken) in
            
            if wasTokenRefreshed, let newToken = newToken {
                
                // Change the global headers
                requestBuilder.openAPIClient.customHeaders["Authorization"] = "Bearer \(newToken)"
                
                completion(.retry)
            } else {
                // If the token was not refreshed, it's because it was not a 401 error, so we send the response to the completion block
                completion(.dontRetry)
            }
        }
    }
    
    private var bearerToken: String? = nil
    
    func refreshTokenIfDoesntExist(completionHandler: @escaping (String) -> Void) {
        if let bearerToken = bearerToken {
            completionHandler(bearerToken)
        } else {
            startRefreshingToken { token in
                completionHandler(token)
            }
        }
    }
    
    func refreshTokenIfUnauthorizedRequestResponse(data: Data?, response: URLResponse?, error: Error, completionHandler: @escaping (Bool, String?) -> Void) {
        if let response = response as? HTTPURLResponse, response.statusCode == 401 {
            startRefreshingToken { token in
                completionHandler(true, token)
            }
        } else {
            completionHandler(false, nil)
        }
    }
    
    private func startRefreshingToken(completionHandler: @escaping (String) -> Void) {
        // Get a bearer token
        let dummyBearerToken = "..."
        
        bearerToken = dummyBearerToken

        completionHandler(dummyBearerToken)
    }
}
```

Then you assign the `BearerOpenAPIInterceptor` to the property `OpenAPIClient.shared.interceptor`.

`OpenAPIClient.shared.interceptor = BearerOpenAPIInterceptor()`

Here is a working sample that put's together all of this.
[AppDelegate.swift](https://github.com/OpenAPITools/openapi-generator/blob/master/samples/client/petstore/swift6/urlsessionLibrary/SwaggerClientTests/SwaggerClient/AppDelegate.swift)
[BearerTokenHandler.swift](https://github.com/OpenAPITools/openapi-generator/blob/master/samples/client/petstore/swift5/urlsessionLibrary/SwaggerClientTests/SwaggerClient/BearerDecodableRequestBuilder.swift)

### How do I implement bearer token authentication with Alamofire on the Swift 6 API client?

First implement the `Alamofire` `RequestInterceptor` protocol.
```
class BearerTokenHandler: RequestInterceptor, @unchecked Sendable {
    private var bearerToken: String? = nil
    
    func adapt(_ urlRequest: URLRequest, for session: Session, completion: @escaping (Result<URLRequest, Error>) -> Void) {
        if let bearerToken = bearerToken {
            var urlRequest = urlRequest
            urlRequest.setValue("Bearer \(bearerToken)", forHTTPHeaderField: "Authorization")
            
            completion(.success(urlRequest))
            return
        }
        
        completion(.success(urlRequest))
    }
    
    func retry(_ request: Request, for session: Session, dueTo error: Error, completion: @escaping (RetryResult) -> Void) {
        if let response = request.task?.response as? HTTPURLResponse, response.statusCode == 401 {
            startRefreshingToken { isTokenRefreshed in
                completion(.retry)
            }
        } else {
            completion(.doNotRetryWithError(error))
        }
    }
    
    private func startRefreshingToken(completionHandler: @escaping (Bool) -> Void) {
        // Get a bearer token
        let dummyBearerToken = "..."
        
        bearerToken = dummyBearerToken
        OpenAPIClient.shared.customHeaders["Authorization"] = "Bearer \(dummyBearerToken)"
        
        completionHandler(true)
    }
}
```

Then you assign the `BearerTokenHandler` to the property `OpenAPIClient.shared.interceptor`.

`OpenAPIClient.shared.interceptor = BearerTokenHandler()`

Here is a working sample that put's together all of this.
[AppDelegate.swift](https://github.com/OpenAPITools/openapi-generator/blob/master/samples/client/petstore/swift6/alamofireLibrary/SwaggerClientTests/SwaggerClient/AppDelegate.swift)
[BearerTokenHandler.swift](https://github.com/OpenAPITools/openapi-generator/blob/master/samples/client/petstore/swift6/alamofireLibrary/SwaggerClientTests/SwaggerClient/BearerTokenHandler.swift)

### How do I migrate from the Swift 5 generator to the swift 6 generator?

- Change the generator to the new `swift6` generator, e.g. `openapi-generator generate -i https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml -g swift6 -o /tmp/test/`
- Check the `swift6` [URLSession](https://github.com/OpenAPITools/openapi-generator/tree/master/samples/client/petstore/swift6/urlsessionLibrary) and [Alamofire](https://github.com/OpenAPITools/openapi-generator/tree/master/samples/client/petstore/swift6/alamofireLibrary) samples.
- The infrastructure files have been moved to a new directory called `Infrastructure`. Please delete the old ones.
- The `AnyCodable` dependency has been removed and replaced with a new enum called `JSONValue`, allowing you to use this generator without external dependencies.
- The `Combine` response is now deferred by default, meaning the request will only start when you begin listening to it. To restore the previous behavior, set the `combineDeferred` flag to `false`.
- A new configuration, `apiStaticMethod`, allows you to use instance methods instead of class methods for API calls. For more information, check the sample project [apiNonStaticMethod](https://github.com/OpenAPITools/openapi-generator/tree/master/samples/client/petstore/swift6/apiNonStaticMethod).
- The new default response is based on async/await. To revert to the previous behavior, set the `responseAs` flag to `ObjcBlock`.
- The default project structure now follows the SPM (Swift Package Manager) structure. To revert to the old structure, set the `useSPMFileStructure` flag to `false`.
- The former `{{projectName}}API` is now called `{{projectName}}APIConfiguration`.
- You can now set a request interceptor and retrier by configuring `OpenAPIClient.shared.interceptor`, making authenticated requests easier to manage.
- The protocol `JSONEncodable` was renamed to `ParameterConvertible`.
- The flag `useJSONEncodable` was renamed to `useParameterConvertible`.

## TypeScript

### The JSON response fails to deserialize due to change in variable naming (snake_case to camelCase). Is there any way to keep the original naming?

Yes, please use the following option when generating TypeScript clients:

```
    modelPropertyNaming
        Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name (Default: camelCase)
```
