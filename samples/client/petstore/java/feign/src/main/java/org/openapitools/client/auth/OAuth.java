package org.openapitools.client.auth;

import java.io.IOException;
import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.oltu.oauth2.client.HttpClient;
import org.apache.oltu.oauth2.client.OAuthClient;
import org.apache.oltu.oauth2.client.request.OAuthClientRequest;
import org.apache.oltu.oauth2.client.request.OAuthClientRequest.AuthenticationRequestBuilder;
import org.apache.oltu.oauth2.client.request.OAuthClientRequest.TokenRequestBuilder;
import org.apache.oltu.oauth2.client.response.OAuthClientResponse;
import org.apache.oltu.oauth2.client.response.OAuthClientResponseFactory;
import org.apache.oltu.oauth2.client.response.OAuthJSONAccessTokenResponse;
import org.apache.oltu.oauth2.common.exception.OAuthProblemException;
import org.apache.oltu.oauth2.common.exception.OAuthSystemException;
import org.apache.oltu.oauth2.common.message.types.GrantType;
import org.apache.oltu.oauth2.common.token.BasicOAuthToken;

import feign.Client;
import feign.Request.Options;
import feign.RequestInterceptor;
import feign.RequestTemplate;
import feign.Response;
import feign.RetryableException;
import feign.Util;
import org.openapitools.client.StringUtil;


public class OAuth implements RequestInterceptor {

    static final int MILLIS_PER_SECOND = 1000;

    public interface AccessTokenListener {
        void notify(BasicOAuthToken token);
    }

    private volatile String accessToken;
    private Long expirationTimeMillis;
    private OAuthClient oauthClient;
    private TokenRequestBuilder tokenRequestBuilder;
    private AuthenticationRequestBuilder authenticationRequestBuilder;
    private AccessTokenListener accessTokenListener;

    public OAuth(Client client, TokenRequestBuilder requestBuilder) {
        this.oauthClient = new OAuthClient(new OAuthFeignClient(client));
        this.tokenRequestBuilder = requestBuilder;
    }

    public OAuth(Client client, OAuthFlow flow, String authorizationUrl, String tokenUrl, String scopes) {
        this(client, OAuthClientRequest.tokenLocation(tokenUrl).setScope(scopes));

        switch(flow) {
        case accessCode:
        case implicit:
            tokenRequestBuilder.setGrantType(GrantType.AUTHORIZATION_CODE);
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
        authenticationRequestBuilder = OAuthClientRequest.authorizationLocation(authorizationUrl);
    }

    public OAuth(OAuthFlow flow, String authorizationUrl, String tokenUrl, String scopes) {
        this(new Client.Default(null, null), flow, authorizationUrl, tokenUrl, scopes);
    }

    @Override
    public void apply(RequestTemplate template) {
        // If the request already have an authorization (eg. Basic auth), do nothing
        if (template.headers().containsKey("Authorization")) {
            return;
        }
        // If first time, get the token
        if (expirationTimeMillis == null || System.currentTimeMillis() >= expirationTimeMillis) {
            updateAccessToken();
        }
        if (getAccessToken() != null) {
            template.header("Authorization", "Bearer " + getAccessToken());
        }
    }

    public synchronized void updateAccessToken() {
        OAuthJSONAccessTokenResponse accessTokenResponse;
        try {
            accessTokenResponse = oauthClient.accessToken(tokenRequestBuilder.buildBodyMessage());
        } catch (Exception e) {
            throw new RetryableException(e.getMessage(), e,null);
        }
        if (accessTokenResponse != null && accessTokenResponse.getAccessToken() != null) {
            setAccessToken(accessTokenResponse.getAccessToken(), accessTokenResponse.getExpiresIn());
            if (accessTokenListener != null) {
                accessTokenListener.notify((BasicOAuthToken) accessTokenResponse.getOAuthToken());
            }
        }
    }

    public synchronized void registerAccessTokenListener(AccessTokenListener accessTokenListener) {
        this.accessTokenListener = accessTokenListener;
    }

    public synchronized String getAccessToken() {
        return accessToken;
    }

    public synchronized void setAccessToken(String accessToken, Long expiresIn) {
        this.accessToken = accessToken;
        this.expirationTimeMillis = System.currentTimeMillis() + expiresIn * MILLIS_PER_SECOND;
    }

    public TokenRequestBuilder getTokenRequestBuilder() {
        return tokenRequestBuilder;
    }

    public void setTokenRequestBuilder(TokenRequestBuilder tokenRequestBuilder) {
        this.tokenRequestBuilder = tokenRequestBuilder;
    }

    public AuthenticationRequestBuilder getAuthenticationRequestBuilder() {
        return authenticationRequestBuilder;
    }

    public void setAuthenticationRequestBuilder(AuthenticationRequestBuilder authenticationRequestBuilder) {
        this.authenticationRequestBuilder = authenticationRequestBuilder;
    }

    public OAuthClient getOauthClient() {
        return oauthClient;
    }

    public void setOauthClient(OAuthClient oauthClient) {
        this.oauthClient = oauthClient;
    }

    public void setOauthClient(Client client) {
        this.oauthClient = new OAuthClient( new OAuthFeignClient(client));
    }

    public static class OAuthFeignClient implements HttpClient {

        private Client client;

        public OAuthFeignClient() {
            this.client = new Client.Default(null, null);
        }

        public OAuthFeignClient(Client client) {
            this.client = client;
        }

        public <T extends OAuthClientResponse> T execute(OAuthClientRequest request, Map<String, String> headers,
                String requestMethod, Class<T> responseClass)
                        throws OAuthSystemException, OAuthProblemException {

            RequestTemplate req = new RequestTemplate()
                    .append(request.getLocationUri())
                    .method(requestMethod)
                    .body(request.getBody());

            for (Entry<String, String> entry : headers.entrySet()) {
                req.header(entry.getKey(), entry.getValue());
            }
            Response feignResponse;
            String body = "";
            try {
                feignResponse = client.execute(req.request(), new Options());
                body = Util.toString(feignResponse.body().asReader());
            } catch (IOException e) {
                throw new OAuthSystemException(e);
            }

            String contentType = null;
            Collection<String> contentTypeHeader =  feignResponse.headers().get("Content-Type");
            if(contentTypeHeader != null) {
                contentType = StringUtil.join(contentTypeHeader.toArray(new String[0]), ";");
            }

            return OAuthClientResponseFactory.createCustomResponse(
                    body,
                    contentType,
                    feignResponse.status(),
                    responseClass
            );
        }

        public void shutdown() {
            // Nothing to do here
        }
    }
}
