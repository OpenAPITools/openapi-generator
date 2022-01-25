package org.openapitools.client.auth;

import com.github.scribejava.core.builder.ServiceBuilder;
import com.github.scribejava.core.model.OAuth2AccessToken;
import com.github.scribejava.core.model.OAuth2Authorization;
import com.github.scribejava.core.oauth.AccessTokenRequestParams;
import com.github.scribejava.core.oauth.OAuth20Service;
import com.github.scribejava.httpclient.okhttp.OkHttpHttpClient;
import okhttp3.OkHttpClient;

import java.io.IOException;
import java.util.Map;
import java.util.concurrent.ExecutionException;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class OAuthAuthorizationCodeGrant extends OAuth {
    private DefaultApi20Impl apiInstance;
    private OAuth20Service service;
    private Map<String, String> params;
    public OAuthAuthorizationCodeGrant(String clientId, String clientSecret, String authorizationBaseUrl, String accessTokenEndpoint, String callbackUrl, Map<String, String> params) {
        this(clientId, clientSecret, null, authorizationBaseUrl, accessTokenEndpoint, callbackUrl, null, params, null);
    }

    public OAuthAuthorizationCodeGrant(String clientId, String clientSecret, String scope, String authorizationBaseUrl, String accessTokenEndpoint, String callbackUrl, Map<String, String> params) {
        this(clientId, clientSecret, scope, authorizationBaseUrl, accessTokenEndpoint, callbackUrl, null, params, null);
    }

    public OAuthAuthorizationCodeGrant(String clientId, String clientSecret, String scope, String authorizationBaseUrl, String accessTokenEndpoint, String callbackUrl, String refreshUrl, Map<String, String> params) {
        this(clientId, clientSecret, scope, authorizationBaseUrl, accessTokenEndpoint, callbackUrl, refreshUrl, params, null);
    }

    public OAuthAuthorizationCodeGrant(String clientId, String clientSecret, String scope, String authorizationBaseUrl, String accessTokenEndpoint, String callbackUrl, String refreshUrl, Map<String, String> params, OkHttpClient httpClient) {
        this.apiInstance = new DefaultApi20Impl(accessTokenEndpoint, authorizationBaseUrl, refreshUrl);
        ServiceBuilder serviceBuilder = new ServiceBuilder(clientId)
                .apiSecret(clientSecret)
                .callback(callbackUrl);
        if (scope != null && !scope.trim().isEmpty()) {
            serviceBuilder.defaultScope(scope);
        }

        if (httpClient != null) {
            serviceBuilder.httpClient(new OkHttpHttpClient(httpClient));
        } else {
            serviceBuilder.httpClient(new OkHttpHttpClient(new OkHttpClient()));
        }


        this.service = serviceBuilder.build(this.apiInstance);

        this.params = params;
    }

    public String getAuthorizationUrl(String state) {
        if (state != null && !state.trim().isEmpty()) {
            return this.service.getAuthorizationUrl(state);
        } else {
            return this.service.getAuthorizationUrl();
        }
    }

    public OAuth2Authorization extractAuthorization(String redirectLocation) {
        return service.extractAuthorization(redirectLocation);
    }

    public OAuth2AccessToken obtainAccessToken(OAuth2Authorization authorization, String scope) throws IOException, ExecutionException, InterruptedException {
        return this.obtainAccessToken(authorization.getCode(), scope);
    }

    public OAuth2AccessToken obtainAccessToken(String code, String scope) throws IOException, ExecutionException, InterruptedException {
        AccessTokenRequestParams reqParams = new AccessTokenRequestParams(code);
        reqParams.addExtraParameters(params);
        if (scope != null && !scope.trim().isEmpty()) {
            reqParams.scope(scope);
        }

        OAuth2AccessToken tokenResponse = service.getAccessToken(reqParams);

        this.setAccessToken(tokenResponse.getAccessToken());
        return tokenResponse;
    }


    public void setToken(OAuth2AccessToken token) {
        this.setAccessToken(token.getAccessToken());
    }

    public void setToken(String accessToken) {
        this.setAccessToken(accessToken);
    }

    public OAuth2AccessToken refreshToken(OAuth2AccessToken token, String scope) throws IOException, ExecutionException, InterruptedException {
        return this.refreshToken(token.getRefreshToken(), scope);
    }

    public OAuth2AccessToken refreshToken(String refreshToken, String scope) throws IOException, ExecutionException, InterruptedException {
        if (scope != null && !scope.trim().isEmpty()) {
            return this.service.refreshAccessToken(refreshToken, scope);
        } else {
            return this.service.refreshAccessToken(refreshToken);
        }
    }

}
