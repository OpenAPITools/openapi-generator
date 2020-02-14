package org.openapitools.client;

import java.io.IOException;
import java.lang.annotation.Annotation;
import java.lang.reflect.Type;
import java.util.*;

import retrofit2.Retrofit;
import retrofit2.converter.scalars.ScalarsConverterFactory;
import retrofit2.converter.jackson.JacksonConverterFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.openapitools.jackson.nullable.JsonNullableModule;

import play.libs.Json;
import play.libs.ws.WSClient;

import org.openapitools.client.Play24CallAdapterFactory;
import org.openapitools.client.Play24CallFactory;

import okhttp3.Interceptor;
import org.openapitools.client.auth.ApiKeyAuth;
import org.openapitools.client.auth.Authentication;

/**
 * API client
 */
public class ApiClient {

    /** Underlying HTTP-client */
    private WSClient wsClient;

    /** Supported auths */
    private Map<String, Authentication> authentications;

    /** API base path */
    private String basePath = "http://petstore.swagger.io:80/v2";

    public ApiClient(WSClient wsClient) {
        this();
        this.wsClient = wsClient;
    }

    public ApiClient() {
        // Setup authentications (key: authentication name, value: authentication).
        authentications = new HashMap<>();
        authentications.put("api_key", new ApiKeyAuth("header", "api_key"));
        authentications.put("api_key_query", new ApiKeyAuth("query", "api_key_query"));
        // authentications.put("http_basic_test", new HttpBasicAuth());
        // authentications.put("petstore_auth", new OAuth());
        // Prevent the authentications from being modified.
        authentications = Collections.unmodifiableMap(authentications);

    }

    /**
     * Creates a retrofit2 client for given API interface
     */
    public <S> S createService(Class<S> serviceClass) {
        if(!basePath.endsWith("/")) {
            basePath = basePath + "/";
        }

        Map<String, String> extraHeaders = new HashMap<>();
        Map<String, String> extraCookies = new HashMap<>();
        List<Pair> extraQueryParams = new ArrayList<>();

        for (String authName : authentications.keySet()) {
            Authentication auth = authentications.get(authName);
            if (auth == null) throw new RuntimeException("Authentication undefined: " + authName);

            auth.applyToParams(extraQueryParams, extraHeaders, extraCookies);
        }

        ObjectMapper mapper = Json.mapper();
        JsonNullableModule jnm = new JsonNullableModule();
        mapper.registerModule(jnm);

        return new Retrofit.Builder()
                       .baseUrl(basePath)
                       .addConverterFactory(ScalarsConverterFactory.create())
                       .addConverterFactory(JacksonConverterFactory.create(mapper))
                       .callFactory(new Play24CallFactory(wsClient, extraHeaders, extraCookies, extraQueryParams))
                       .addCallAdapterFactory(new Play24CallAdapterFactory())
                       .build()
                       .create(serviceClass);
    }

    /**
     * Helper method to set API base path
     */
    public ApiClient setBasePath(String basePath) {
        this.basePath = basePath;
        return this;
    }

    /**
     * Get authentications (key: authentication name, value: authentication).
     */
    public Map<String, Authentication> getAuthentications() {
        return authentications;
    }

    /**
     * Get authentication for the given name.
     *
     * @param authName The authentication name
     * @return The authentication, null if not found
     */
    public Authentication getAuthentication(String authName) {
        return authentications.get(authName);
    }

    /**
     * Helper method to set API key value for the first API key authentication.
     */
    public ApiClient setApiKey(String apiKey) {
        for (Authentication auth : authentications.values()) {
            if (auth instanceof ApiKeyAuth) {
                ((ApiKeyAuth) auth).setApiKey(apiKey);
                return this;
            }
        }

        throw new RuntimeException("No API key authentication configured!");
    }

    /**
     * Helper method to set API key prefix for the first API key authentication.
     */
    public ApiClient setApiKeyPrefix(String apiKeyPrefix) {
        for (Authentication auth : authentications.values()) {
            if (auth instanceof ApiKeyAuth) {
                ((ApiKeyAuth) auth).setApiKeyPrefix(apiKeyPrefix);
                return this;
            }
        }

        throw new RuntimeException("No API key authentication configured!");
    }


}
