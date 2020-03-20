package org.openapitools.client.infrastructure

import okhttp3.Interceptor
import okhttp3.OkHttpClient
import retrofit2.Retrofit
import retrofit2.converter.scalars.ScalarsConverterFactory
import com.squareup.moshi.Moshi
import retrofit2.converter.moshi.MoshiConverterFactory

class ApiClient(
    private var baseUrl: String = BASE_URL,
    private val okHttpClientBuilder: OkHttpClient.Builder? = null,
    private val serializerBuilder: Moshi.Builder = Serializer.moshiBuilder
) {
    private val apiAuthorizations = mutableMapOf<String, Interceptor>()

    private val retrofitBuilder: Retrofit.Builder by lazy {
        Retrofit.Builder()
            .baseUrl(baseUrl)
            .addConverterFactory(ScalarsConverterFactory.create())
            .addConverterFactory(MoshiConverterFactory.create(Serializer.moshi))
    }

    private val clientBuilder: OkHttpClient.Builder by lazy {
        okHttpClientBuilder ?: defaultClientBuilder
    }

    private val defaultClientBuilder: OkHttpClient.Builder by lazy {
        OkHttpClient().newBuilder()
    }

    init {
        normalizeBaseUrl()
    }

    constructor(
        baseUrl: String = BASE_URL,
        okHttpClientBuilder: OkHttpClient.Builder? = null,
        serializerBuilder: Moshi.Builder = Serializer.moshiBuilder,
        authNames: Array<String>
    ) : this(baseUrl, okHttpClientBuilder, serializerBuilder) {
        authNames.forEach { authName ->
            val auth = when (authName) {
                "api_key" -> "petstore_auth" -> 
                else -> throw RuntimeException("auth name $authName not found in available auth names")
            }
            addAuthorization(authName, auth);
        }
    }

    /**
     * Adds an authorization to be used by the client
     * @param authName Authentication name
     * @param authorization Authorization interceptor
     * @return ApiClient
     */
    fun addAuthorization(authName: String, authorization: Interceptor): ApiClient {
        if (apiAuthorizations.containsKey(authName)) {
            throw RuntimeException("auth name $authName already in api authorizations")
        }
        apiAuthorizations[authName] = authorization
        clientBuilder.addInterceptor(authorization)
        return this
    }

    fun <S> createService(serviceClass: Class<S>): S {
        return retrofitBuilder.client(clientBuilder.build()).build().create(serviceClass)
    }

    private fun normalizeBaseUrl() {
        if (!baseUrl.endsWith("/")) {
            baseUrl += "/"
        }
    }

    private inline fun <T, reified U> Iterable<T>.firstAs(): U {
        for (element in this) if (element is U) return element
        throw NoSuchElementException("Collection contains no element for generic parameter U")
    }

    companion object {
        const val BASE_URL: String = "http://petstore.swagger.io/v2"
    }
}