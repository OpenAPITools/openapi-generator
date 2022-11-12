# org.openapitools.client - Kotlin client library for Demo


A kotlin client for Android using the currently recommended http client, Volley. See https://developer.android.com/training/volley

- Currently sends GsonRequests
- Currently only supports Gson as a serializer - will throw an exception if a different serializer is chosen
- Defaults the source location to src/main/java as per standard Android builds


## Design

Volley is a queue/request based layer on top of http url stack specific to Android. Android favours dependency injection and
a layered architecture, and IO performed off the main thread to maintain UI responsiveness, with a preferred technique of
kotlin co-routines. The code gen library reflects these factors.

- Api calls use co-routines, and execute them using volley callbacks to avoid tying up a thread.
- Facilitate dependency injection, with default implementations available.
- Generate a requestFactory that can be overridden
- Allow the passing of the RequestFactory per tag (api client) or per operation (an extra parameter is created on operations with non-global security), with per operation auth overriding global security.
- DI scoping of the Request Factory and pre-generated auth header factories allow for thread safe and secure setting of credentials.
- Lazy header factories allow for refreshing tokens etc
- Factoring of header factories to the Request Factory allow ambient provision of credentials. Code gen library is credential storage agnostic.
- Header factories allow the merging of generated headers from open api spec with dynamically added headers

- Injection of http url stack to allow custom http stacks. Default implementation is best practice singleton
- Data classes used for serialisation to reflect volley's preference - an immutable request that once queued can't be tampered with.

- Reuse model class and other jvm common infrastructure

- Optional generation of room database models, and transform methods to these from open api models
- Room and api models can be extended with additional extension properties.

## Future improvements
- Option to generate image requests on certain conditionals e.g content-type gif etc
- Support for kotlin serialization.
- Multi part form parameters and support for file inputs

## Usage
Hilt Dependency injection example - with default values for parameters overridden.
```
 @Provides
    internal fun provideSomeApi(
        context: Context,
        restService: IRestService,
        configurationService: IConfigurationService,
        sessionService: ISessionService
    ): SomeApi {
        return SomeApi(
            context = context,
            requestQueue = restService.getRequestQueue(),
            requestFactory = RequestFactory(listOf(createSessionHeaderFactory(sessionService), createTraceHeaderFactory()),
                postProcessors = listOf(retryPolicySetter)),
            basePath = configurationService.getBaseUrl()
        )
    }
```
Here is the constructor so you can see the defaults
```class SomeApi (
val context: Context,
val requestQueue: Lazy<RequestQueue> = lazy(initializer = {
    Volley.newRequestQueue(context.applicationContext)
    }),
    val requestFactory: IRequestFactory = RequestFactory(),
    val basePath: String = "https://yourbasepath.from_input_parameter.com/api",
    private val postProcessors :List <(Request<*>) -> Unit> = listOf()) {
```

### Overriding defaults
The above constructor for each api allows the following to be customized
- A custom context, so either a singleton request queue or different scope can be created - see
https://developer.android.com/training/volley/requestqueue#singleton
- An overridable request queue - which in turn can have a custom http url stack passed to it
- An overridable request factory constructor call, or a request factory that can be overridden by a custom template, with
custom header factory, request post processors and custom gson adapters injected.

#### Overriding request generation
Request generation can be overridden by
- Overriding the entire request factory template
- Supplying custom header factories - methods that take any possible parameters but return a map of headers
- Supplying custom request post processors - methods that take and return the request object

Header factory examples can be found in the auth section, as these are implemented as header factories. eg
```
val basicAuthHeaderFactoryBuilder = { username: String?, password: String? ->
{ mapOf("Authorization" to "Basic " + Base64.encodeToString("${username ?: ""}:${password ?: ""}".toByteArray(), Base64.DEFAULT))}
}
```
In this case it's a lambda function (a factory method) that takes an username and password, and returns a map of headers. Other
generated code will supply the username and password. In this case it results in a map of  just one key/value pair, but
it could be multiple. The important part is it's returning a map - and that the surrounding code
will can bind the inputs to it at some point.

Here is a different example that supplies tracing header values
```
/**
 * Create a lambda of tracing headers to be injected into an API's [RequestFactory].
 */
private fun createTraceHeaderFactory(): () -> Map<String, String> = {
    mapOf(
        HttpHeaderType.b3_traceId.rawValue to UUIDExtensions.asTraceId(UUID.randomUUID()),
        HttpHeaderType.b3_spanId.rawValue to UUIDExtensions.asSpanId(UUID.randomUUID()),
        HttpHeaderType.b3_sampled.rawValue to "1"
    )
}
```
Finally a post processor example
```
 /**
     * Configure a [DefaultRetryPolicy] to be injected into the [RequestFactory] with a maximum number of retries of zero.
     */
    private val retryPolicySetter = { request: Request<*> ->
        Unit.apply {
            request.setRetryPolicy(
                DefaultRetryPolicy(
                    RestService.DEFAULT_TIMEOUT_MS,
                    0,
                    DefaultRetryPolicy.DEFAULT_BACKOFF_MULT
                )
            )
        }
    }
```

### Serialization
#### Gson and Polymorphic types
The GsonRequest object can be passed custom type adapters
```
class GsonRequest<T>(
    method: Int,
    url: String,
    private val body: Any?,
    private val headers: Map<String, String>?,
    private val params: MutableMap<String, String>?,
    private val contentTypeForBody: String?,
    private val encodingForParams: String?,
    private val gsonAdapters: Map<Type, Object>?,
    private val type: Type,
    private val listener: Response.Listener<T>,
    errorListener: Response.ErrorListener
) : Request<T>(method, url, errorListener) {

    val gsonBuilder: GsonBuilder = GsonBuilder()
        .registerTypeAdapter(OffsetDateTime::class.java, OffsetDateTimeAdapter())
        .registerTypeAdapter(LocalDateTime::class.java, LocalDateTimeAdapter())
        .registerTypeAdapter(LocalDate::class.java, LocalDateAdapter())
        .registerTypeAdapter(ByteArray::class.java, ByteArrayAdapter())

```
## Requires

* Kotlin 1.4.30
* Gradle 6.8.3

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

<a name="documentation-for-api-endpoints"></a>
## Documentation for API Endpoints

All URIs are relative to *http://localhost*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*DefaultApi* | [**idsGet**](docs/DefaultApi.md#idsget) | **GET** /{ids} | 


<a name="documentation-for-models"></a>
## Documentation for Models



<a name="documentation-for-authorization"></a>
## Documentation for Authorization

All endpoints do not require authorization.
