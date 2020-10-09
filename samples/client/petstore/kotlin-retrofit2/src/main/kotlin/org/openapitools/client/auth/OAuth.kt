package org.openapitools.client.auth

import java.net.HttpURLConnection.HTTP_UNAUTHORIZED
import java.net.HttpURLConnection.HTTP_FORBIDDEN

import java.io.IOException

import org.apache.oltu.oauth2.client.OAuthClient
import org.apache.oltu.oauth2.client.request.OAuthBearerClientRequest
import org.apache.oltu.oauth2.client.request.OAuthClientRequest
import org.apache.oltu.oauth2.client.request.OAuthClientRequest.AuthenticationRequestBuilder
import org.apache.oltu.oauth2.client.request.OAuthClientRequest.TokenRequestBuilder
import org.apache.oltu.oauth2.common.exception.OAuthProblemException
import org.apache.oltu.oauth2.common.exception.OAuthSystemException
import org.apache.oltu.oauth2.common.message.types.GrantType
import org.apache.oltu.oauth2.common.token.BasicOAuthToken

import okhttp3.Interceptor
import okhttp3.OkHttpClient
import okhttp3.Response

class OAuth(
        client: OkHttpClient,
        var tokenRequestBuilder: TokenRequestBuilder
) : Interceptor {

    interface AccessTokenListener {
        fun notify(token: BasicOAuthToken)
    }

    private var oauthClient: OAuthClient = OAuthClient(OAuthOkHttpClient(client))

    @Volatile 
    private var accessToken: String? = null
    var authenticationRequestBuilder: AuthenticationRequestBuilder? = null
    private var accessTokenListener: AccessTokenListener? = null

    constructor(
            requestBuilder: TokenRequestBuilder
    ) : this(
            OkHttpClient(), 
            requestBuilder
    )

    constructor(
            flow: OAuthFlow, 
            authorizationUrl: String, 
            tokenUrl: String, 
            scopes: String
    ) : this(
            OAuthClientRequest.tokenLocation(tokenUrl).setScope(scopes)
    ) {
        setFlow(flow);
        authenticationRequestBuilder = OAuthClientRequest.authorizationLocation(authorizationUrl);
    }

    fun setFlow(flow: OAuthFlow) {
        when (flow) {
            OAuthFlow.accessCode, OAuthFlow.implicit ->
                tokenRequestBuilder.setGrantType(GrantType.AUTHORIZATION_CODE)
            OAuthFlow.password ->
                tokenRequestBuilder.setGrantType(GrantType.PASSWORD)
            OAuthFlow.application ->
                tokenRequestBuilder.setGrantType(GrantType.CLIENT_CREDENTIALS)
        }            
    }

    @Throws(IOException::class)
    override fun intercept(chain: Interceptor.Chain): Response {
        return retryingIntercept(chain, true)
    }

    @Throws(IOException::class)
    private fun retryingIntercept(chain: Interceptor.Chain, updateTokenAndRetryOnAuthorizationFailure: Boolean): Response {
        var request = chain.request()

        // If the request already have an authorization (eg. Basic auth), do nothing
        if (request.header("Authorization") != null) {
            return chain.proceed(request)
        }

        // If first time, get the token
        val oAuthRequest: OAuthClientRequest
        if (accessToken == null) {
            updateAccessToken(null)
        }

        if (accessToken != null) {
            // Build the request
            val rb = request.newBuilder()

            val requestAccessToken = accessToken
            try {
                oAuthRequest = OAuthBearerClientRequest(request.url.toString())
                        .setAccessToken(requestAccessToken)
                        .buildHeaderMessage()
            } catch (e: OAuthSystemException) {
                throw IOException(e)
            }

            oAuthRequest.headers.entries.forEach { header ->
                rb.addHeader(header.key, header.value)
            }
            rb.url(oAuthRequest.locationUri)

            //Execute the request
            val response = chain.proceed(rb.build())

            // 401/403 most likely indicates that access token has expired. Unless it happens two times in a row.
            if ((response.code == HTTP_UNAUTHORIZED || response.code == HTTP_FORBIDDEN) && updateTokenAndRetryOnAuthorizationFailure) {
                try {
                    if (updateAccessToken(requestAccessToken)) {
                        response.body?.close()
                        return retryingIntercept(chain, false)
                    }
                } catch (e: Exception) {
                    response.body?.close()
                    throw e
                }
            }
            return response
        } else {
            return chain.proceed(chain.request())
        }
    }

    /**
     * Returns true if the access token has been updated
     */
    @Throws(IOException::class)
    @Synchronized 
    fun updateAccessToken(requestAccessToken: String?): Boolean {
        if (accessToken == null || accessToken.equals(requestAccessToken)) {    
            return try {
                val accessTokenResponse = oauthClient.accessToken(this.tokenRequestBuilder.buildBodyMessage())
                if (accessTokenResponse != null && accessTokenResponse.accessToken != null) {
                    accessToken = accessTokenResponse.accessToken
                    accessTokenListener?.notify(accessTokenResponse.oAuthToken as BasicOAuthToken)
                    !accessToken.equals(requestAccessToken)
                } else {
                    false
                }
            } catch (e: OAuthSystemException) {
                throw IOException(e)
            } catch (e: OAuthProblemException) {
                throw IOException(e)
            }
        }
        return true;
    }
}
