package org.openapitools.client.auth;

import java.io.IOException;

import okhttp3.Interceptor;
import okhttp3.Request;
import okhttp3.Response;

public class HttpBearerAuth implements Interceptor {
    private final String scheme;
    private String bearerToken;

    public HttpBearerAuth(String scheme) {
        this.scheme = scheme;
    }

    public String getBearerToken() {
        return bearerToken;
    }

    public void setBearerToken(String bearerToken) {
        this.bearerToken = bearerToken;
    }

    @Override
    public Response intercept(Chain chain) throws IOException {
        Request request = chain.request();

        // If the request already have an authorization (eg. Basic auth), do nothing
        if (request.header("Authorization") == null && bearerToken != null) {
            request = request.newBuilder()
                    .addHeader("Authorization", (scheme != null ? upperCaseBearer(scheme) + " " : "") + bearerToken)
                    .build();
        }
        return chain.proceed(request);
    }

    private static String upperCaseBearer(String scheme) {
        return ("bearer".equalsIgnoreCase(scheme)) ? "Bearer" : scheme;
    }

}
