package io.swagger.client.auth;

import feign.RequestInterceptor;
import feign.RequestTemplate;

public class OAuth implements RequestInterceptor {

	public OAuth(OAuthFlow flow, String authorizationUrl, String tokenUrl, String scopes) {
        // TODO Not implemented yet
    }
    
    @Override
    public void apply(RequestTemplate template) {
        // TODO Not implemented yet
    }

}