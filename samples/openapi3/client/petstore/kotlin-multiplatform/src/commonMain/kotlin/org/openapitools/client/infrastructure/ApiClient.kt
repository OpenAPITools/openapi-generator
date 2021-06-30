package org.openapitools.client.infrastructure

import io.ktor.client.HttpClient
import io.ktor.client.HttpClientConfig
import io.ktor.client.call.call
import io.ktor.client.engine.HttpClientEngine
import io.ktor.client.features.json.JsonFeature
import io.ktor.client.features.json.JsonSerializer
import io.ktor.client.features.json.serializer.KotlinxSerializer
import io.ktor.client.request.accept
import io.ktor.client.request.forms.FormDataContent
import io.ktor.client.request.forms.MultiPartFormDataContent
import io.ktor.client.request.header
import io.ktor.client.request.parameter
import io.ktor.client.response.HttpResponse
import io.ktor.client.utils.EmptyContent
import io.ktor.http.*
import io.ktor.http.content.OutgoingContent
import io.ktor.http.content.PartData
import kotlinx.serialization.UnstableDefault
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration

import org.openapitools.client.apis.*
import org.openapitools.client.models.*
import org.openapitools.client.auth.*

open class ApiClient(
        private val baseUrl: String,
        httpClientEngine: HttpClientEngine?,
        serializer: KotlinxSerializer) {

    @UseExperimental(UnstableDefault::class)
    constructor(
            baseUrl: String,
            httpClientEngine: HttpClientEngine?,
            jsonConfiguration: JsonConfiguration) :
            this(baseUrl, httpClientEngine, KotlinxSerializer(Json(jsonConfiguration)))

    private val serializer: JsonSerializer by lazy {
        serializer.apply { setMappers(this) }.ignoreOutgoingContent()
    }

    private val client: HttpClient by lazy {
        val jsonConfig: JsonFeature.Config.() -> Unit = { this.serializer = this@ApiClient.serializer }
        val clientConfig: (HttpClientConfig<*>) -> Unit = { it.install(JsonFeature, jsonConfig) }
        httpClientEngine?.let { HttpClient(it, clientConfig) } ?: HttpClient(clientConfig)
    }
    private val authentications: kotlin.collections.Map<String, Authentication> by lazy {
        mapOf(
                "api_key" to ApiKeyAuth("header", "api_key"), 
                "api_key_query" to ApiKeyAuth("query", "api_key_query"), 
                "bearer_test" to HttpBearerAuth("bearer"), 
                "http_basic_test" to HttpBasicAuth(), 
                "http_signature_test" to HttpBearerAuth("signature"), 
                "petstore_auth" to OAuth())
    }

    companion object {
        protected val UNSAFE_HEADERS = listOf(HttpHeaders.ContentType)

        private fun setMappers(serializer: KotlinxSerializer) {
            
            AnotherFakeApi.setMappers(serializer)
            
            DefaultApi.setMappers(serializer)
            
            FakeApi.setMappers(serializer)
            
            FakeClassnameTags123Api.setMappers(serializer)
            
            PetApi.setMappers(serializer)
            
            StoreApi.setMappers(serializer)
            
            UserApi.setMappers(serializer)
            
            serializer.setMapper(org.openapitools.client.models.AdditionalPropertiesClass::class, org.openapitools.client.models.AdditionalPropertiesClass.serializer())
            serializer.setMapper(org.openapitools.client.models.Animal::class, org.openapitools.client.models.Animal.serializer())
            serializer.setMapper(org.openapitools.client.models.ApiResponse::class, org.openapitools.client.models.ApiResponse.serializer())
            serializer.setMapper(org.openapitools.client.models.ArrayOfArrayOfNumberOnly::class, org.openapitools.client.models.ArrayOfArrayOfNumberOnly.serializer())
            serializer.setMapper(org.openapitools.client.models.ArrayOfNumberOnly::class, org.openapitools.client.models.ArrayOfNumberOnly.serializer())
            serializer.setMapper(org.openapitools.client.models.ArrayTest::class, org.openapitools.client.models.ArrayTest.serializer())
            serializer.setMapper(org.openapitools.client.models.Capitalization::class, org.openapitools.client.models.Capitalization.serializer())
            serializer.setMapper(org.openapitools.client.models.Cat::class, org.openapitools.client.models.Cat.serializer())
            serializer.setMapper(org.openapitools.client.models.CatAllOf::class, org.openapitools.client.models.CatAllOf.serializer())
            serializer.setMapper(org.openapitools.client.models.Category::class, org.openapitools.client.models.Category.serializer())
            serializer.setMapper(org.openapitools.client.models.ClassModel::class, org.openapitools.client.models.ClassModel.serializer())
            serializer.setMapper(org.openapitools.client.models.Client::class, org.openapitools.client.models.Client.serializer())
            serializer.setMapper(org.openapitools.client.models.Dog::class, org.openapitools.client.models.Dog.serializer())
            serializer.setMapper(org.openapitools.client.models.DogAllOf::class, org.openapitools.client.models.DogAllOf.serializer())
            serializer.setMapper(org.openapitools.client.models.EnumArrays::class, org.openapitools.client.models.EnumArrays.serializer())
            serializer.setMapper(org.openapitools.client.models.EnumClass::class, org.openapitools.client.models.EnumClass.Serializer)
            serializer.setMapper(org.openapitools.client.models.EnumTest::class, org.openapitools.client.models.EnumTest.serializer())
            serializer.setMapper(org.openapitools.client.models.FileSchemaTestClass::class, org.openapitools.client.models.FileSchemaTestClass.serializer())
            serializer.setMapper(org.openapitools.client.models.Foo::class, org.openapitools.client.models.Foo.serializer())
            serializer.setMapper(org.openapitools.client.models.FormatTest::class, org.openapitools.client.models.FormatTest.serializer())
            serializer.setMapper(org.openapitools.client.models.HasOnlyReadOnly::class, org.openapitools.client.models.HasOnlyReadOnly.serializer())
            serializer.setMapper(org.openapitools.client.models.HealthCheckResult::class, org.openapitools.client.models.HealthCheckResult.serializer())
            serializer.setMapper(org.openapitools.client.models.InlineResponseDefault::class, org.openapitools.client.models.InlineResponseDefault.serializer())
            serializer.setMapper(org.openapitools.client.models.List::class, org.openapitools.client.models.List.serializer())
            serializer.setMapper(org.openapitools.client.models.MapTest::class, org.openapitools.client.models.MapTest.serializer())
            serializer.setMapper(org.openapitools.client.models.MixedPropertiesAndAdditionalPropertiesClass::class, org.openapitools.client.models.MixedPropertiesAndAdditionalPropertiesClass.serializer())
            serializer.setMapper(org.openapitools.client.models.Model200Response::class, org.openapitools.client.models.Model200Response.serializer())
            serializer.setMapper(org.openapitools.client.models.Name::class, org.openapitools.client.models.Name.serializer())
            serializer.setMapper(org.openapitools.client.models.NullableClass::class, org.openapitools.client.models.NullableClass.serializer())
            serializer.setMapper(org.openapitools.client.models.NumberOnly::class, org.openapitools.client.models.NumberOnly.serializer())
            serializer.setMapper(org.openapitools.client.models.Order::class, org.openapitools.client.models.Order.serializer())
            serializer.setMapper(org.openapitools.client.models.OuterComposite::class, org.openapitools.client.models.OuterComposite.serializer())
            serializer.setMapper(org.openapitools.client.models.OuterEnum::class, org.openapitools.client.models.OuterEnum.Serializer)
            serializer.setMapper(org.openapitools.client.models.OuterEnumDefaultValue::class, org.openapitools.client.models.OuterEnumDefaultValue.Serializer)
            serializer.setMapper(org.openapitools.client.models.OuterEnumInteger::class, org.openapitools.client.models.OuterEnumInteger.Serializer)
            serializer.setMapper(org.openapitools.client.models.OuterEnumIntegerDefaultValue::class, org.openapitools.client.models.OuterEnumIntegerDefaultValue.Serializer)
            serializer.setMapper(org.openapitools.client.models.Pet::class, org.openapitools.client.models.Pet.serializer())
            serializer.setMapper(org.openapitools.client.models.ReadOnlyFirst::class, org.openapitools.client.models.ReadOnlyFirst.serializer())
            serializer.setMapper(org.openapitools.client.models.Return::class, org.openapitools.client.models.Return.serializer())
            serializer.setMapper(org.openapitools.client.models.SpecialModelname::class, org.openapitools.client.models.SpecialModelname.serializer())
            serializer.setMapper(org.openapitools.client.models.Tag::class, org.openapitools.client.models.Tag.serializer())
            serializer.setMapper(org.openapitools.client.models.User::class, org.openapitools.client.models.User.serializer())
        }
    }

    /**
     * Set the username for the first HTTP basic authentication.
     *
     * @param username Username
     */
    fun setUsername(username: String) {
        val auth = authentications?.values?.firstOrNull { it is HttpBasicAuth } as HttpBasicAuth?
                ?: throw Exception("No HTTP basic authentication configured")
        auth.username = username
    }

    /**
     * Set the password for the first HTTP basic authentication.
     *
     * @param password Password
     */
    fun setPassword(password: String) {
        val auth = authentications?.values?.firstOrNull { it is HttpBasicAuth } as HttpBasicAuth?
                ?: throw Exception("No HTTP basic authentication configured")
        auth.password = password
    }

    /**
     * Set the API key value for the first API key authentication.
     *
     * @param apiKey API key
     * @param paramName The name of the API key parameter, or null or set the first key.
     */
    fun setApiKey(apiKey: String, paramName: String? = null) {
        val auth = authentications?.values?.firstOrNull { it is ApiKeyAuth && (paramName == null || paramName == it.paramName)} as ApiKeyAuth?
                ?: throw Exception("No API key authentication configured")
        auth.apiKey = apiKey
    }

    /**
     * Set the API key prefix for the first API key authentication.
     *
     * @param apiKeyPrefix API key prefix
     * @param paramName The name of the API key parameter, or null or set the first key.
     */
    fun setApiKeyPrefix(apiKeyPrefix: String, paramName: String? = null) {
        val auth = authentications?.values?.firstOrNull { it is ApiKeyAuth && (paramName == null || paramName == it.paramName) } as ApiKeyAuth?
                ?: throw Exception("No API key authentication configured")
        auth.apiKeyPrefix = apiKeyPrefix
    }

    /**
     * Set the access token for the first OAuth2 authentication.
     *
     * @param accessToken Access token
     */
    fun setAccessToken(accessToken: String) {
        val auth = authentications?.values?.firstOrNull { it is OAuth } as OAuth?
                ?: throw Exception("No OAuth2 authentication configured")
        auth.accessToken = accessToken
    }

    /**
     * Set the access token for the first Bearer authentication.
     *
     * @param bearerToken The bearer token.
     */
    fun setBearerToken(bearerToken: String) {
        val auth = authentications?.values?.firstOrNull { it is HttpBearerAuth } as HttpBearerAuth?
                ?: throw Exception("No Bearer authentication configured")
        auth.bearerToken = bearerToken
    }

    protected suspend fun multipartFormRequest(requestConfig: RequestConfig, body: kotlin.collections.List<PartData>?, authNames: kotlin.collections.List<String>): HttpResponse {
        return request(requestConfig, MultiPartFormDataContent(body ?: listOf()), authNames)
    }

    protected suspend fun urlEncodedFormRequest(requestConfig: RequestConfig, body: Parameters?, authNames: kotlin.collections.List<String>): HttpResponse {
        return request(requestConfig, FormDataContent(body ?: Parameters.Empty), authNames)
    }

    protected suspend fun jsonRequest(requestConfig: RequestConfig, body: Any? = null, authNames: kotlin.collections.List<String>): HttpResponse {
        val contentType = (requestConfig.headers[HttpHeaders.ContentType]?.let { ContentType.parse(it) }
                ?: ContentType.Application.Json)
        return if (body != null) request(requestConfig, serializer.write(body, contentType), authNames)
        else request(requestConfig, authNames = authNames)
    }

    protected suspend fun request(requestConfig: RequestConfig, body: OutgoingContent = EmptyContent, authNames: kotlin.collections.List<String>): HttpResponse {
        requestConfig.updateForAuth(authNames)
        val headers = requestConfig.headers

        return client.call {
            this.url {
                this.takeFrom(URLBuilder(baseUrl))
                appendPath(requestConfig.path.trimStart('/').split('/'))
                requestConfig.query.forEach { query ->
                    query.value.forEach { value ->
                        parameter(query.key, value)
                    }
                }
            }
            this.method = requestConfig.method.httpMethod
            headers.filter { header -> !UNSAFE_HEADERS.contains(header.key) }.forEach { header -> this.header(header.key, header.value) }
            if (requestConfig.method in listOf(RequestMethod.PUT, RequestMethod.POST, RequestMethod.PATCH))
                this.body = body

        }.response
    }

    private fun RequestConfig.updateForAuth(authNames: kotlin.collections.List<String>) {
        for (authName in authNames) {
            val auth = authentications?.get(authName) ?: throw Exception("Authentication undefined: $authName")
            auth.apply(query, headers)
        }
    }

    private fun URLBuilder.appendPath(components: kotlin.collections.List<String>): URLBuilder = apply {
        encodedPath = encodedPath.trimEnd('/') + components.joinToString("/", prefix = "/") { it.encodeURLQueryComponent() }
    }

    private val RequestMethod.httpMethod: HttpMethod
        get() = when (this) {
            RequestMethod.DELETE -> HttpMethod.Delete
            RequestMethod.GET -> HttpMethod.Get
            RequestMethod.HEAD -> HttpMethod.Head
            RequestMethod.PATCH -> HttpMethod.Patch
            RequestMethod.PUT -> HttpMethod.Put
            RequestMethod.POST -> HttpMethod.Post
            RequestMethod.OPTIONS -> HttpMethod.Options
        }
}

// https://github.com/ktorio/ktor/issues/851
private fun JsonSerializer.ignoreOutgoingContent() = IgnoreOutgoingContentJsonSerializer(this)

private class IgnoreOutgoingContentJsonSerializer(private val delegate: JsonSerializer) : JsonSerializer by delegate {
    override fun write(data: Any): OutgoingContent {
        if (data is OutgoingContent) return data
        return delegate.write(data)
    }
}
