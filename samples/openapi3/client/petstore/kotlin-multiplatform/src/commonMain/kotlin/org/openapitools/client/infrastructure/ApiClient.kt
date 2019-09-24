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
            
            serializer.setMapper(AdditionalPropertiesClass::class, AdditionalPropertiesClass.serializer())
            serializer.setMapper(Animal::class, Animal.serializer())
            serializer.setMapper(ApiResponse::class, ApiResponse.serializer())
            serializer.setMapper(ArrayOfArrayOfNumberOnly::class, ArrayOfArrayOfNumberOnly.serializer())
            serializer.setMapper(ArrayOfNumberOnly::class, ArrayOfNumberOnly.serializer())
            serializer.setMapper(ArrayTest::class, ArrayTest.serializer())
            serializer.setMapper(Capitalization::class, Capitalization.serializer())
            serializer.setMapper(Cat::class, Cat.serializer())
            serializer.setMapper(CatAllOf::class, CatAllOf.serializer())
            serializer.setMapper(Category::class, Category.serializer())
            serializer.setMapper(ClassModel::class, ClassModel.serializer())
            serializer.setMapper(Client::class, Client.serializer())
            serializer.setMapper(Dog::class, Dog.serializer())
            serializer.setMapper(DogAllOf::class, DogAllOf.serializer())
            serializer.setMapper(EnumArrays::class, EnumArrays.serializer())
            serializer.setMapper(EnumClass::class, EnumClass.serializer())
            serializer.setMapper(EnumTest::class, EnumTest.serializer())
            serializer.setMapper(FileSchemaTestClass::class, FileSchemaTestClass.serializer())
            serializer.setMapper(Foo::class, Foo.serializer())
            serializer.setMapper(FormatTest::class, FormatTest.serializer())
            serializer.setMapper(HasOnlyReadOnly::class, HasOnlyReadOnly.serializer())
            serializer.setMapper(HealthCheckResult::class, HealthCheckResult.serializer())
            serializer.setMapper(InlineObject::class, InlineObject.serializer())
            serializer.setMapper(InlineObject1::class, InlineObject1.serializer())
            serializer.setMapper(InlineObject2::class, InlineObject2.serializer())
            serializer.setMapper(InlineObject3::class, InlineObject3.serializer())
            serializer.setMapper(InlineObject4::class, InlineObject4.serializer())
            serializer.setMapper(InlineObject5::class, InlineObject5.serializer())
            serializer.setMapper(InlineResponseDefault::class, InlineResponseDefault.serializer())
            serializer.setMapper(List::class, List.serializer())
            serializer.setMapper(MapTest::class, MapTest.serializer())
            serializer.setMapper(MixedPropertiesAndAdditionalPropertiesClass::class, MixedPropertiesAndAdditionalPropertiesClass.serializer())
            serializer.setMapper(Model200Response::class, Model200Response.serializer())
            serializer.setMapper(Name::class, Name.serializer())
            serializer.setMapper(NullableClass::class, NullableClass.serializer())
            serializer.setMapper(NumberOnly::class, NumberOnly.serializer())
            serializer.setMapper(Order::class, Order.serializer())
            serializer.setMapper(OuterComposite::class, OuterComposite.serializer())
            serializer.setMapper(OuterEnum::class, OuterEnum.serializer())
            serializer.setMapper(OuterEnumDefaultValue::class, OuterEnumDefaultValue.serializer())
            serializer.setMapper(OuterEnumInteger::class, OuterEnumInteger.serializer())
            serializer.setMapper(OuterEnumIntegerDefaultValue::class, OuterEnumIntegerDefaultValue.serializer())
            serializer.setMapper(Pet::class, Pet.serializer())
            serializer.setMapper(ReadOnlyFirst::class, ReadOnlyFirst.serializer())
            serializer.setMapper(Return::class, Return.serializer())
            serializer.setMapper(SpecialModelname::class, SpecialModelname.serializer())
            serializer.setMapper(Tag::class, Tag.serializer())
            serializer.setMapper(User::class, User.serializer())
        }
    }

    protected suspend fun multipartFormRequest(requestConfig: RequestConfig, body: List<PartData>?): HttpResponse {
        return request(requestConfig, MultiPartFormDataContent(body ?: listOf()))
    }

    protected suspend fun urlEncodedFormRequest(requestConfig: RequestConfig, body: Parameters?): HttpResponse {
        return request(requestConfig, FormDataContent(body ?: Parameters.Empty))
    }

    protected suspend fun jsonRequest(requestConfig: RequestConfig, body: Any? = null): HttpResponse {
        val contentType = (requestConfig.headers[HttpHeaders.ContentType]?.let { ContentType.parse(it) }
                ?: ContentType.Application.Json)
        return if (body != null) request(requestConfig, serializer.write(body, contentType))
        else request(requestConfig)
    }

    protected suspend fun request(requestConfig: RequestConfig, body: OutgoingContent = EmptyContent): HttpResponse {
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

    private fun URLBuilder.appendPath(components: List<String>): URLBuilder = apply {
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
