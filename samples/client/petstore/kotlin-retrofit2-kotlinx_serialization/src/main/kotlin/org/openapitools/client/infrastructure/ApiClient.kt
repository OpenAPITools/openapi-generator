package org.openapitools.client.infrastructure

import org.apache.oltu.oauth2.client.request.OAuthClientRequest.AuthenticationRequestBuilder
import org.apache.oltu.oauth2.client.request.OAuthClientRequest.TokenRequestBuilder
import org.openapitools.client.auth.ApiKeyAuth
import org.openapitools.client.auth.OAuth
import org.openapitools.client.auth.OAuth.AccessTokenListener
import org.openapitools.client.auth.OAuthFlow

import okhttp3.Call
import okhttp3.Interceptor
import okhttp3.OkHttpClient
import retrofit2.Retrofit
import okhttp3.logging.HttpLoggingInterceptor
import retrofit2.Converter
import retrofit2.converter.scalars.ScalarsConverterFactory

import com.jakewharton.retrofit2.converter.kotlinx.serialization.asConverterFactory
import org.openapitools.client.infrastructure.Serializer.jvmJson
import okhttp3.MediaType.Companion.toMediaType

class ApiClient(
    private var baseUrl: String = defaultBasePath,
    private val okHttpClientBuilder: OkHttpClient.Builder? = null,
    private val callFactory : Call.Factory? = null,
    private val converterFactory: Converter.Factory? = null,
) {
    private val apiAuthorizations = mutableMapOf<String, Interceptor>()
    var logger: ((String) -> Unit)? = null

    private val retrofitBuilder: Retrofit.Builder by lazy {
        Retrofit.Builder()
            .baseUrl(baseUrl)
            .addConverterFactory(ScalarsConverterFactory.create())
            .addConverterFactory(jvmJson.asConverterFactory("application/json".toMediaType()))
            .apply {
                if (converterFactory != null) {
                    addConverterFactory(converterFactory)
                }
            }
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
        
        authNames: Array<String>
    ) : this(baseUrl, okHttpClientBuilder) {
        authNames.forEach { authName ->
            val auth = when (authName) {
                "api_key" -> ApiKeyAuth("header", "api_key")"petstore_auth" -> OAuth(OAuthFlow.implicit, "http://petstore.swagger.io/api/oauth/dialog", "", "write:pets, read:pets")
                else -> throw RuntimeException("auth name $authName not found in available auth names")
            }
            addAuthorization(authName, auth);
        }
    }

    constructor(
        baseUrl: String = defaultBasePath,
        okHttpClientBuilder: OkHttpClient.Builder? = null,
        
        authName: String,
        clientId: String,
        secret: String,
        username: String,
        password: String
    ) : this(baseUrl, okHttpClientBuilder, arrayOf(authName)) {
        getTokenEndPoint()
            ?.setClientId(clientId)
            ?.setClientSecret(secret)
            ?.setUsername(username)
            ?.setPassword(password)
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
        val usedCallFactory = this.callFactory ?: clientBuilder.build()
        return retrofitBuilder.callFactory(usedCallFactory).build().create(serviceClass)
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
        protected val baseUrlKey = "org.openapitools.client.baseUrl"

        @JvmStatic
        val defaultBasePath: String by lazy {
            System.getProperties().getProperty(baseUrlKey, "http://petstore.swagger.io/v2")
        }
    }
}
