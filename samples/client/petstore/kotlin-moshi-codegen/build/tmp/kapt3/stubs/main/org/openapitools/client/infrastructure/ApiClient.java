package org.openapitools.client.infrastructure;

import java.lang.System;

@kotlin.Metadata(mv = {1, 1, 16}, bv = {1, 0, 3}, k = 1, d1 = {"\u00008\n\u0002\u0018\u0002\n\u0002\u0010\u0000\n\u0000\n\u0002\u0010\u000e\n\u0002\b\b\n\u0002\u0018\u0002\n\u0000\n\u0002\u0018\u0002\n\u0002\b\u0002\n\u0002\u0018\u0002\n\u0002\b\u0004\n\u0002\u0018\u0002\n\u0002\b\u0002\n\u0002\u0010\u0002\n\u0002\b\u0002\b\u0016\u0018\u0000 \u001a2\u00020\u0001:\u0001\u001aB\r\u0012\u0006\u0010\u0002\u001a\u00020\u0003\u00a2\u0006\u0002\u0010\u0004J\"\u0010\u0007\u001a\u00020\u0003\"\n\b\u0000\u0010\b\u0018\u0001*\u00020\u00012\u0006\u0010\t\u001a\u0002H\bH\u0084\b\u00a2\u0006\u0002\u0010\nJ-\u0010\u000b\u001a\n\u0012\u0006\u0012\u0004\u0018\u0001H\b0\f\"\u0006\b\u0000\u0010\b\u0018\u00012\u0006\u0010\r\u001a\u00020\u000e2\n\b\u0002\u0010\u000f\u001a\u0004\u0018\u00010\u0001H\u0084\bJ(\u0010\u0010\u001a\u00020\u0011\"\u0006\b\u0000\u0010\b\u0018\u00012\u0006\u0010\u0012\u001a\u0002H\b2\b\b\u0002\u0010\u0013\u001a\u00020\u0003H\u0084\b\u00a2\u0006\u0002\u0010\u0014J.\u0010\u0015\u001a\u0004\u0018\u0001H\b\"\u0006\b\u0000\u0010\b\u0018\u00012\b\u0010\u000f\u001a\u0004\u0018\u00010\u00162\n\b\u0002\u0010\u0013\u001a\u0004\u0018\u00010\u0003H\u0084\b\u00a2\u0006\u0002\u0010\u0017J\u0010\u0010\u0018\u001a\u00020\u00192\u0006\u0010\r\u001a\u00020\u000eH\u0004R\u0011\u0010\u0002\u001a\u00020\u0003\u00a2\u0006\b\n\u0000\u001a\u0004\b\u0005\u0010\u0006\u00a8\u0006\u001b"}, d2 = {"Lorg/openapitools/client/infrastructure/ApiClient;", "", "baseUrl", "", "(Ljava/lang/String;)V", "getBaseUrl", "()Ljava/lang/String;", "parseDateToQueryString", "T", "value", "(Ljava/lang/Object;)Ljava/lang/String;", "request", "Lorg/openapitools/client/infrastructure/ApiInfrastructureResponse;", "requestConfig", "Lorg/openapitools/client/infrastructure/RequestConfig;", "body", "requestBody", "Lokhttp3/RequestBody;", "content", "mediaType", "(Ljava/lang/Object;Ljava/lang/String;)Lokhttp3/RequestBody;", "responseBody", "Lokhttp3/ResponseBody;", "(Lokhttp3/ResponseBody;Ljava/lang/String;)Ljava/lang/Object;", "updateAuthParams", "", "Companion", "kotlin-petstore-moshi-codegen"})
public class ApiClient {
    @org.jetbrains.annotations.NotNull()
    private final java.lang.String baseUrl = null;
    @org.jetbrains.annotations.NotNull()
    protected static final java.lang.String ContentType = "Content-Type";
    @org.jetbrains.annotations.NotNull()
    protected static final java.lang.String Accept = "Accept";
    @org.jetbrains.annotations.NotNull()
    protected static final java.lang.String Authorization = "Authorization";
    @org.jetbrains.annotations.NotNull()
    protected static final java.lang.String JsonMediaType = "application/json";
    @org.jetbrains.annotations.NotNull()
    protected static final java.lang.String FormDataMediaType = "multipart/form-data";
    @org.jetbrains.annotations.NotNull()
    protected static final java.lang.String FormUrlEncMediaType = "application/x-www-form-urlencoded";
    @org.jetbrains.annotations.NotNull()
    protected static final java.lang.String XmlMediaType = "application/xml";
    @org.jetbrains.annotations.NotNull()
    private static final java.util.Map<java.lang.String, java.lang.String> apiKey = null;
    @org.jetbrains.annotations.NotNull()
    private static final java.util.Map<java.lang.String, java.lang.String> apiKeyPrefix = null;
    @org.jetbrains.annotations.Nullable()
    private static java.lang.String username;
    @org.jetbrains.annotations.Nullable()
    private static java.lang.String password;
    @org.jetbrains.annotations.Nullable()
    private static java.lang.String accessToken;
    @org.jetbrains.annotations.NotNull()
    private static final kotlin.Lazy client$delegate = null;
    @org.jetbrains.annotations.NotNull()
    private static final okhttp3.OkHttpClient.Builder builder = null;
    public static final org.openapitools.client.infrastructure.ApiClient.Companion Companion = null;
    
    protected final void updateAuthParams(@org.jetbrains.annotations.NotNull()
    org.openapitools.client.infrastructure.RequestConfig requestConfig) {
    }
    
    @org.jetbrains.annotations.NotNull()
    public final java.lang.String getBaseUrl() {
        return null;
    }
    
    public ApiClient(@org.jetbrains.annotations.NotNull()
    java.lang.String baseUrl) {
        super();
    }
    
    @org.jetbrains.annotations.NotNull()
    public static final okhttp3.OkHttpClient getClient() {
        return null;
    }
    
    @org.jetbrains.annotations.NotNull()
    public static final okhttp3.OkHttpClient.Builder getBuilder() {
        return null;
    }
    
    @kotlin.Metadata(mv = {1, 1, 16}, bv = {1, 0, 3}, k = 1, d1 = {"\u0000,\n\u0002\u0018\u0002\n\u0002\u0010\u0000\n\u0002\b\u0002\n\u0002\u0010\u000e\n\u0002\b\f\n\u0002\u0010%\n\u0002\b\u0005\n\u0002\u0018\u0002\n\u0002\b\u0004\n\u0002\u0018\u0002\n\u0002\b\f\b\u0086\u0003\u0018\u00002\u00020\u0001B\u0007\b\u0002\u00a2\u0006\u0002\u0010\u0002R\u000e\u0010\u0003\u001a\u00020\u0004X\u0084T\u00a2\u0006\u0002\n\u0000R\u000e\u0010\u0005\u001a\u00020\u0004X\u0084T\u00a2\u0006\u0002\n\u0000R\u000e\u0010\u0006\u001a\u00020\u0004X\u0084T\u00a2\u0006\u0002\n\u0000R\u000e\u0010\u0007\u001a\u00020\u0004X\u0084T\u00a2\u0006\u0002\n\u0000R\u000e\u0010\b\u001a\u00020\u0004X\u0084T\u00a2\u0006\u0002\n\u0000R\u000e\u0010\t\u001a\u00020\u0004X\u0084T\u00a2\u0006\u0002\n\u0000R\u000e\u0010\n\u001a\u00020\u0004X\u0084T\u00a2\u0006\u0002\n\u0000R\u001c\u0010\u000b\u001a\u0004\u0018\u00010\u0004X\u0086\u000e\u00a2\u0006\u000e\n\u0000\u001a\u0004\b\f\u0010\r\"\u0004\b\u000e\u0010\u000fR\u001d\u0010\u0010\u001a\u000e\u0012\u0004\u0012\u00020\u0004\u0012\u0004\u0012\u00020\u00040\u0011\u00a2\u0006\b\n\u0000\u001a\u0004\b\u0012\u0010\u0013R\u001d\u0010\u0014\u001a\u000e\u0012\u0004\u0012\u00020\u0004\u0012\u0004\u0012\u00020\u00040\u0011\u00a2\u0006\b\n\u0000\u001a\u0004\b\u0015\u0010\u0013R\u001c\u0010\u0016\u001a\u00020\u00178\u0006X\u0087\u0004\u00a2\u0006\u000e\n\u0000\u0012\u0004\b\u0018\u0010\u0002\u001a\u0004\b\u0019\u0010\u001aR!\u0010\u001b\u001a\u00020\u001c8FX\u0087\u0084\u0002\u00a2\u0006\u0012\n\u0004\b \u0010!\u0012\u0004\b\u001d\u0010\u0002\u001a\u0004\b\u001e\u0010\u001fR\u001c\u0010\"\u001a\u0004\u0018\u00010\u0004X\u0086\u000e\u00a2\u0006\u000e\n\u0000\u001a\u0004\b#\u0010\r\"\u0004\b$\u0010\u000fR\u001c\u0010%\u001a\u0004\u0018\u00010\u0004X\u0086\u000e\u00a2\u0006\u000e\n\u0000\u001a\u0004\b&\u0010\r\"\u0004\b\'\u0010\u000f\u00a8\u0006("}, d2 = {"Lorg/openapitools/client/infrastructure/ApiClient$Companion;", "", "()V", "Accept", "", "Authorization", "ContentType", "FormDataMediaType", "FormUrlEncMediaType", "JsonMediaType", "XmlMediaType", "accessToken", "getAccessToken", "()Ljava/lang/String;", "setAccessToken", "(Ljava/lang/String;)V", "apiKey", "", "getApiKey", "()Ljava/util/Map;", "apiKeyPrefix", "getApiKeyPrefix", "builder", "Lokhttp3/OkHttpClient$Builder;", "builder$annotations", "getBuilder", "()Lokhttp3/OkHttpClient$Builder;", "client", "Lokhttp3/OkHttpClient;", "client$annotations", "getClient", "()Lokhttp3/OkHttpClient;", "client$delegate", "Lkotlin/Lazy;", "password", "getPassword", "setPassword", "username", "getUsername", "setUsername", "kotlin-petstore-moshi-codegen"})
    public static final class Companion {
        
        @org.jetbrains.annotations.NotNull()
        public final java.util.Map<java.lang.String, java.lang.String> getApiKey() {
            return null;
        }
        
        @org.jetbrains.annotations.NotNull()
        public final java.util.Map<java.lang.String, java.lang.String> getApiKeyPrefix() {
            return null;
        }
        
        @org.jetbrains.annotations.Nullable()
        public final java.lang.String getUsername() {
            return null;
        }
        
        public final void setUsername(@org.jetbrains.annotations.Nullable()
        java.lang.String p0) {
        }
        
        @org.jetbrains.annotations.Nullable()
        public final java.lang.String getPassword() {
            return null;
        }
        
        public final void setPassword(@org.jetbrains.annotations.Nullable()
        java.lang.String p0) {
        }
        
        @org.jetbrains.annotations.Nullable()
        public final java.lang.String getAccessToken() {
            return null;
        }
        
        public final void setAccessToken(@org.jetbrains.annotations.Nullable()
        java.lang.String p0) {
        }
        
        public static void client$annotations() {
        }
        
        @org.jetbrains.annotations.NotNull()
        public final okhttp3.OkHttpClient getClient() {
            return null;
        }
        
        public static void builder$annotations() {
        }
        
        @org.jetbrains.annotations.NotNull()
        public final okhttp3.OkHttpClient.Builder getBuilder() {
            return null;
        }
        
        private Companion() {
            super();
        }
    }
}