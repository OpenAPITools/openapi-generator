package org.openapitools.client.auth;

import com.github.scribejava.core.builder.ServiceBuilder;
import com.github.scribejava.core.model.OAuth2AccessToken;
import com.github.scribejava.core.oauth.OAuth20Service;
import com.github.scribejava.httpclient.okhttp.OkHttpHttpClient;
import okhttp3.OkHttpClient;

import java.io.IOException;
import java.util.concurrent.ExecutionException;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class OAuthClientCredentialsGrant extends OAuth {
    private DefaultApi20Impl apiInstance;
    private OAuth20Service service;

    public OAuthClientCredentialsGrant(String clientId, String clientSecret, String accessTokenEndpoint) {
        this(clientId, clientSecret, null, accessTokenEndpoint, null);
    }

    public OAuthClientCredentialsGrant(String clientId, String clientSecret, String scope, String accessTokenEndpoint) {
        this(clientId, clientSecret, scope, accessTokenEndpoint, null);
    }

    public OAuthClientCredentialsGrant(String clientId, String clientSecret, String scope, String accessTokenEndpoint, OkHttpClient httpClient) {
        this.apiInstance = new DefaultApi20Impl(accessTokenEndpoint, null, null);
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

    public OAuth2AccessToken obtainAccessToken(String scope) throws IOException, ExecutionException, InterruptedException {
        OAuth2AccessToken tokenResponse;
        if (scope != null && !scope.trim().isEmpty()) {
            tokenResponse = service.getAccessTokenClientCredentialsGrant(scope);
        } else {
            tokenResponse = service.getAccessTokenClientCredentialsGrant();
        }

        this.setAccessToken(tokenResponse.getAccessToken());

        return tokenResponse;
    }
    
    public void setToken(OAuth2AccessToken token) {
        this.setToken(token.getAccessToken());
    }

    public void setToken(String accessToken) {
        this.setAccessToken(accessToken);
    }

}
