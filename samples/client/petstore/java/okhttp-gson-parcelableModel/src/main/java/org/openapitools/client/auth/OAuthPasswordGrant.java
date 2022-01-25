package org.openapitools.client.auth;

import com.github.scribejava.core.builder.ServiceBuilder;
import com.github.scribejava.core.model.OAuth2AccessToken;
import com.github.scribejava.core.oauth.OAuth20Service;
import com.github.scribejava.httpclient.okhttp.OkHttpHttpClient;
import okhttp3.OkHttpClient;

import java.io.IOException;
import java.util.concurrent.ExecutionException;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class OAuthPasswordGrant extends OAuth {
    private DefaultApi20Impl apiInstance;
    private OAuth20Service service;
    public OAuthPasswordGrant(String clientId, String clientSecret, String accessTokenEndpoint) {
        this(clientId, clientSecret, null, accessTokenEndpoint, null, null);
    }

    public OAuthPasswordGrant(String clientId, String clientSecret, String scope, String accessTokenEndpoint) {
        this(clientId, clientSecret, scope, accessTokenEndpoint, null, null);
    }

    public OAuthPasswordGrant(String clientId, String clientSecret, String scope, String accessTokenEndpoint, String refreshTokenUrl) {
        this(clientId, clientSecret, scope, accessTokenEndpoint, refreshTokenUrl, null);
    }

    public OAuthPasswordGrant(String clientId, String clientSecret, String scope, String accessTokenEndpoint, String refreshTokenUrl, OkHttpClient httpClient) {
        this.apiInstance = new DefaultApi20Impl(accessTokenEndpoint, null, refreshTokenUrl);
        ServiceBuilder serviceBuilder = new ServiceBuilder(clientId)
                .apiSecret(clientSecret);
        if (scope != null && !scope.trim().isEmpty()) {
            serviceBuilder.defaultScope(scope);
        }

        if (httpClient != null) {
            serviceBuilder.httpClient(new OkHttpHttpClient(httpClient));
        } else {
            serviceBuilder.httpClient(new OkHttpHttpClient(new OkHttpClient()));
        }

        this.service = serviceBuilder.build(this.apiInstance);
    }

    public OAuth2AccessToken obtainAccessToken(String username, String password) throws IOException, ExecutionException, InterruptedException {
        return this.obtainAccessToken(username, password, null);
    }

    public OAuth2AccessToken obtainAccessToken(String username, String password, String scope) throws IOException, ExecutionException, InterruptedException {
        OAuth2AccessToken tokenResponse;
        if (scope != null && !scope.trim().isEmpty()) {
            tokenResponse = service.getAccessTokenPasswordGrant(username, password, scope);
        } else {
            tokenResponse = service.getAccessTokenPasswordGrant(username, password);
        }

        this.setAccessToken(tokenResponse.getAccessToken());

        return tokenResponse;
    }

    public void setToken(OAuth2AccessToken token) {
        this.setToken(token.getAccessToken());
    }

    public void setToken(String accessToken) {
        this.setToken(accessToken);
    }

    public OAuth2AccessToken refreshToken(OAuth2AccessToken token) throws IOException, ExecutionException, InterruptedException {
        return this.refreshToken(token.getRefreshToken());
    }

    public OAuth2AccessToken refreshToken(String refreshToken) throws IOException, ExecutionException, InterruptedException {
        return this.service.refreshAccessToken(refreshToken);
    }

}
