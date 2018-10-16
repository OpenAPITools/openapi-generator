package org.openapitools.client.auth;

import com.squareup.okhttp.Interceptor;
import com.squareup.okhttp.OkHttpClient;
import com.squareup.okhttp.Request;
import com.squareup.okhttp.Response;

import org.apache.oltu.oauth2.client.OAuthClient;
import org.apache.oltu.oauth2.client.request.OAuthBearerClientRequest;
import org.apache.oltu.oauth2.client.request.OAuthClientRequest;
import org.apache.oltu.oauth2.client.request.OAuthClientRequest.TokenRequestBuilder;
import org.apache.oltu.oauth2.client.response.OAuthJSONAccessTokenResponse;
import org.apache.oltu.oauth2.common.exception.OAuthProblemException;
import org.apache.oltu.oauth2.common.exception.OAuthSystemException;
import org.apache.oltu.oauth2.common.message.types.GrantType;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Map;

public class RetryingOAuth extends OAuth implements Interceptor {
    private OAuthClient oAuthClient;

    private TokenRequestBuilder tokenRequestBuilder;

    public RetryingOAuth(OkHttpClient client, TokenRequestBuilder tokenRequestBuilder) {
        this.oAuthClient = new OAuthClient(new OAuthOkHttpClient(client));
        this.tokenRequestBuilder = tokenRequestBuilder;
    }

    public RetryingOAuth(TokenRequestBuilder tokenRequestBuilder) {
        this(new OkHttpClient(), tokenRequestBuilder);
    }

    public RetryingOAuth(
            String tokenUrl,
            String clientId,
            OAuthFlow flow,
            String clientSecret,
            Map<String, String> parameters
    ) {
        this(OAuthClientRequest.tokenLocation(tokenUrl)
                .setClientId(clientId)
                .setClientSecret(clientSecret));
        setFlow(flow);
        if (parameters != null) {
            for (String paramName : parameters.keySet()) {
                tokenRequestBuilder.setParameter(paramName, parameters.get(paramName));
            }
        }
    }

    public void setFlow(OAuthFlow flow) {
        switch(flow) {
            case accessCode:
                tokenRequestBuilder.setGrantType(GrantType.AUTHORIZATION_CODE);
                break;
            case implicit:
                tokenRequestBuilder.setGrantType(GrantType.IMPLICIT);
                break;
            case password:
                tokenRequestBuilder.setGrantType(GrantType.PASSWORD);
                break;
            case application:
                tokenRequestBuilder.setGrantType(GrantType.CLIENT_CREDENTIALS);
                break;
            default:
                break;
        }
    }

    @Override
    public Response intercept(Chain chain) throws IOException {
        return retryingIntercept(chain, true);
    }

    private Response retryingIntercept(Chain chain, boolean updateTokenAndRetryOnAuthorizationFailure) throws IOException {
        Request request = chain.request();

        // If the request already has an authorization (e.g. Basic auth), proceed with the request as is
        if (request.header("Authorization") != null) {
            return chain.proceed(request);
        }

        // Get the token if it has not yet been acquired
        if (getAccessToken() == null) {
            updateAccessToken(null);
        }

        OAuthClientRequest oAuthRequest;
        if (getAccessToken() != null) {
            // Build the request
            Request.Builder requestBuilder = request.newBuilder();

            String requestAccessToken = getAccessToken();
            try {
                oAuthRequest =
                        new OAuthBearerClientRequest(request.urlString()).
                                setAccessToken(requestAccessToken).
                                buildHeaderMessage();
            } catch (OAuthSystemException e) {
                throw new IOException(e);
            }

            Map<String, String> headers = oAuthRequest.getHeaders();
            for (String headerName : headers.keySet()) {
                requestBuilder.addHeader(headerName, headers.get(headerName));
            }
            requestBuilder.url(oAuthRequest.getLocationUri());

            // Execute the request
            Response response = chain.proceed(requestBuilder.build());

            // 401/403 response codes most likely indicate an expired access token, unless it happens two times in a row
            if (
                    response != null &&
                            (   response.code() == HttpURLConnection.HTTP_UNAUTHORIZED ||
                                    response.code() == HttpURLConnection.HTTP_FORBIDDEN     ) &&
                            updateTokenAndRetryOnAuthorizationFailure
            ) {
                try {
                    if (updateAccessToken(requestAccessToken)) {
                        response.body().close();
                        return retryingIntercept(chain, false);
                    }
                } catch (Exception e) {
                    response.body().close();
                    throw e;
                }
            }
            return response;
        }
        else {
            return chain.proceed(chain.request());
        }
    }

    /*
     * Returns true if the access token has been updated
     */
    public synchronized boolean updateAccessToken(String requestAccessToken) throws IOException {
        if (getAccessToken() == null || getAccessToken().equals(requestAccessToken)) {
            try {
                OAuthJSONAccessTokenResponse accessTokenResponse =
                        oAuthClient.accessToken(tokenRequestBuilder.buildBodyMessage());
                if (accessTokenResponse != null && accessTokenResponse.getAccessToken() != null) {
                    setAccessToken(accessTokenResponse.getAccessToken());
                    return !getAccessToken().equals(requestAccessToken);
                }
            } catch (OAuthSystemException | OAuthProblemException e) {
                throw new IOException(e);
            }
        }

        return false;
    }

    public TokenRequestBuilder getTokenRequestBuilder() {
        return tokenRequestBuilder;
    }

    public void setTokenRequestBuilder(TokenRequestBuilder tokenRequestBuilder) {
        this.tokenRequestBuilder = tokenRequestBuilder;
    }
}
