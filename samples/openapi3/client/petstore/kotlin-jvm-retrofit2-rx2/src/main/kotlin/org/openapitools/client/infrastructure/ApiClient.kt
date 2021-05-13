package org.openapitools.client.infrastructure

import org.apache.oltu.oauth2.client.request.OAuthClientRequest.AuthenticationRequestBuilder
import org.apache.oltu.oauth2.client.request.OAuthClientRequest.TokenRequestBuilder
import org.openapitools.client.auth.ApiKeyAuth
import org.openapitools.client.auth.OAuth
import org.openapitools.client.auth.OAuth.AccessTokenListener
import org.openapitools.client.auth.OAuthFlow
import org.openapitools.client.auth.HttpBearerAuth
import org.openapitools.client.auth.HttpBasicAuth

import okhttp3.Interceptor
import okhttp3.OkHttpClient
import retrofit2.Retrofit
import okhttp3.logging.HttpLoggingInterceptor
import retrofit2.converter.scalars.ScalarsConverterFactory
import retrofit2.adapter.rxjava2.RxJava2CallAdapterFactory
import com.google.gson.Gson
import com.google.gson.GsonBuilder
import retrofit2.converter.gson.GsonConverterFactory

class ApiClient(
    private var baseUrl: String = defaultBasePath,
    private val okHttpClientBuilder: OkHttpClient.Builder? = null,
    private val serializerBuilder: GsonBuilder = Serializer.gsonBuilder,
    private val okHttpClient : OkHttpClient? = null
) {
    private val apiAuthorizations = mutableMapOf<String, Interceptor>()
    var logger: ((String) -> Unit)? = null

    private val retrofitBuilder: Retrofit.Builder by lazy {
        Retrofit.Builder()
            .baseUrl(baseUrl)
            .addConverterFactory(ScalarsConverterFactory.create())
            .addConverterFactory(GsonConverterFactory.create(serializerBuilder.create()))
            .addCallAdapterFactory(RxJava2CallAdapterFactory.create())
                }

    private val clientBuilder: OkHttpClient.Builder by lazy {
        okHttpClientBuilder ?: defaultClientBuilder
    }

    private val defaultClientBuilder: OkHttpClient.Builder by lazy {
        OkHttpClient()
            .newBuilder()
            .addInterceptor(HttpLoggingInterceptor(object : HttpLoggingInterceptor.Logger {
                override fun log(message: String) {
                    logger?.invoke(message)
                }
            }).apply {
                level = HttpLoggingInterceptor.Level.BODY
            })
    }

    init {
        normalizeBaseUrl()
    }

    constructor(
        baseUrl: String = defaultBasePath,
        okHttpClientBuilder: OkHttpClient.Builder? = null,
        serializerBuilder: GsonBuilder = Serializer.gsonBuilder,
        authNames: Array<String>
    ) : this(baseUrl, okHttpClientBuilder, serializerBuilder) {
        authNames.forEach { authName ->
            val auth = when (authName) {
                "api_key" -> ApiKeyAuth("header", "api_key")"api_key_query" -> ApiKeyAuth("query", "api_key_query")"bearer_test" -> HttpBearerAuth("bearer")"http_basic_test" -> HttpBasicAuth()"http_signature_test" -> "petstore_auth" -> OAuth(OAuthFlow.implicit, "http://petstore.swagger.io/api/oauth/dialog", "", "write:pets, read:pets")
                else -> throw RuntimeException("auth name $authName not found in available auth names")
            }
            addAuthorization(authName, auth);
        }
    }

    constructor(
        baseUrl: String = defaultBasePath,
        okHttpClientBuilder: OkHttpClient.Builder? = null,
        serializerBuilder: GsonBuilder = Serializer.gsonBuilder,
        authName: String,
        bearerToken: String
    ) : this(baseUrl, okHttpClientBuilder, serializerBuilder, arrayOf(authName)) {
        setBearerToken(bearerToken)
    }

    constructor(
        baseUrl: String = defaultBasePath,
        okHttpClientBuilder: OkHttpClient.Builder? = null,
        serializerBuilder: GsonBuilder = Serializer.gsonBuilder,
        authName: String,
        username: String,
        password: String
    ) : this(baseUrl, okHttpClientBuilder, serializerBuilder, arrayOf(authName)) {
        setCredentials(username, password)
    }

    constructor(
        baseUrl: String = defaultBasePath,
        okHttpClientBuilder: OkHttpClient.Builder? = null,
        serializerBuilder: GsonBuilder = Serializer.gsonBuilder,
        authName: String,
        clientId: String,
        secret: String,
        username: String,
        password: String
    ) : this(baseUrl, okHttpClientBuilder, serializerBuilder, arrayOf(authName)) {
        getTokenEndPoint()
            ?.setClientId(clientId)
            ?.setClientSecret(secret)
            ?.setUsername(username)
            ?.setPassword(password)
    }

    fun setCredentials(username: String, password: String): ApiClient {
        apiAuthorizations.values.runOnFirst<Interceptor, OAuth> {
            tokenRequestBuilder.setUsername(username).setPassword(password)
        }
        return this
    }
    fun setBearerToken(bearerToken: String): ApiClient {
        apiAuthorizations.values.runOnFirst<Interceptor, HttpBearerAuth> {
            this.bearerToken = bearerToken
        }
        return this
    }

    fun setCredentials(username: String, password: String): ApiClient {
        apiAuthorizations.values.runOnFirst<Interceptor, HttpBasicAuth> {
            setCredentials(username, password);
        }
        apiAuthorizations.values.runOnFirst<Interceptor, OAuth> {
            tokenRequestBuilder.setUsername(username).setPassword(password)
        }
        return this
    }

    fun setCredentials(username: String, password: String): ApiClient {
        apiAuthorizations.values.runOnFirst<Interceptor, OAuth> {
            tokenRequestBuilder.setUsername(username).setPassword(password)
        }
        return this
    }
    /**
    * Helper method to configure the token endpoint of the first oauth found in the apiAuthorizations (there should be only one)
    * @return Token request builder
    */
    fun getTokenEndPoint(): TokenRequestBuilder? {
        var result: TokenRequestBuilder? = null
        apiAuthorizations.values.runOnFirst<Interceptor, OAuth> {
            result = tokenRequestBuilder
        }
        return result
    }

    /**
    * Helper method to configure authorization endpoint of the first oauth found in the apiAuthorizations (there should be only one)
    * @return Authentication request builder
    */
    fun getAuthorizationEndPoint(): AuthenticationRequestBuilder? {
        var result: AuthenticationRequestBuilder? = null
        apiAuthorizations.values.runOnFirst<Interceptor, OAuth> {
            result = authenticationRequestBuilder
        }
        return result
    }

    /**
    * Helper method to pre-set the oauth access token of the first oauth found in the apiAuthorizations (there should be only one)
    * @param accessToken Access token
    * @return ApiClient
    */
    fun setAccessToken(accessToken: String): ApiClient {
        apiAuthorizations.values.runOnFirst<Interceptor, OAuth> {
            setAccessToken(accessToken)
        }
        return this
    }

    /**
    * Helper method to configure the oauth accessCode/implicit flow parameters
    * @param clientId Client ID
    * @param clientSecret Client secret
    * @param redirectURI Redirect URI
    * @return ApiClient
    */
    fun configureAuthorizationFlow(clientId: String, clientSecret: String, redirectURI: String): ApiClient {
        apiAuthorizations.values.runOnFirst<Interceptor, OAuth> {
            tokenRequestBuilder
                .setClientId(clientId)
                .setClientSecret(clientSecret)
                .setRedirectURI(redirectURI)
            authenticationRequestBuilder
                ?.setClientId(clientId)
                ?.setRedirectURI(redirectURI)
        }
        return this;
    }

    /**
    * Configures a listener which is notified when a new access token is received.
    * @param accessTokenListener Access token listener
    * @return ApiClient
    */
    fun registerAccessTokenListener(accessTokenListener: AccessTokenListener): ApiClient {
        apiAuthorizations.values.runOnFirst<Interceptor, OAuth> {
            registerAccessTokenListener(accessTokenListener)
        }
        return this;
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

    fun setLogger(logger: (String) -> Unit): ApiClient {
        this.logger = logger
        return this
    }

    fun <S> createService(serviceClass: Class<S>): S {
        val usedClient = this.okHttpClient ?: clientBuilder.build()
        return retrofitBuilder.client(usedClient).build().create(serviceClass)
    }

    private fun normalizeBaseUrl() {
        if (!baseUrl.endsWith("/")) {
            baseUrl += "/"
        }
    }

    private inline fun <T, reified U> Iterable<T>.runOnFirst(callback: U.() -> Unit) {
        for (element in this) {
            if (element is U)  {
                callback.invoke(element)
                break
            }
        }
    }

    companion object {
        @JvmStatic
        val defaultBasePath: String by lazy {
            System.getProperties().getProperty("org.openapitools.client.baseUrl", "http://petstore.swagger.io:80/v2")
        }
    }
}
