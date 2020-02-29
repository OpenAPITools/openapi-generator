package org.openapitools.client.infrastructure;

import java.lang.System;

/**
 * Defines a config object for a given request.
 * NOTE: This object doesn't include 'body' because it
 *      allows for caching of the constructed object
 *      for many request definitions.
 * NOTE: Headers is a Map<String,String> because rfc2616 defines
 *      multi-valued headers as csv-only.
 */
@kotlin.Metadata(mv = {1, 1, 16}, bv = {1, 0, 3}, k = 1, d1 = {"\u00004\n\u0002\u0018\u0002\n\u0002\u0010\u0000\n\u0000\n\u0002\u0018\u0002\n\u0000\n\u0002\u0010\u000e\n\u0000\n\u0002\u0010%\n\u0000\n\u0002\u0010 \n\u0002\b\u000e\n\u0002\u0010\u000b\n\u0002\b\u0002\n\u0002\u0010\b\n\u0002\b\u0002\b\u0086\b\u0018\u00002\u00020\u0001BG\u0012\u0006\u0010\u0002\u001a\u00020\u0003\u0012\u0006\u0010\u0004\u001a\u00020\u0005\u0012\u0014\b\u0002\u0010\u0006\u001a\u000e\u0012\u0004\u0012\u00020\u0005\u0012\u0004\u0012\u00020\u00050\u0007\u0012\u001a\b\u0002\u0010\b\u001a\u0014\u0012\u0004\u0012\u00020\u0005\u0012\n\u0012\b\u0012\u0004\u0012\u00020\u00050\t0\u0007\u00a2\u0006\u0002\u0010\nJ\t\u0010\u0012\u001a\u00020\u0003H\u00c6\u0003J\t\u0010\u0013\u001a\u00020\u0005H\u00c6\u0003J\u0015\u0010\u0014\u001a\u000e\u0012\u0004\u0012\u00020\u0005\u0012\u0004\u0012\u00020\u00050\u0007H\u00c6\u0003J\u001b\u0010\u0015\u001a\u0014\u0012\u0004\u0012\u00020\u0005\u0012\n\u0012\b\u0012\u0004\u0012\u00020\u00050\t0\u0007H\u00c6\u0003JO\u0010\u0016\u001a\u00020\u00002\b\b\u0002\u0010\u0002\u001a\u00020\u00032\b\b\u0002\u0010\u0004\u001a\u00020\u00052\u0014\b\u0002\u0010\u0006\u001a\u000e\u0012\u0004\u0012\u00020\u0005\u0012\u0004\u0012\u00020\u00050\u00072\u001a\b\u0002\u0010\b\u001a\u0014\u0012\u0004\u0012\u00020\u0005\u0012\n\u0012\b\u0012\u0004\u0012\u00020\u00050\t0\u0007H\u00c6\u0001J\u0013\u0010\u0017\u001a\u00020\u00182\b\u0010\u0019\u001a\u0004\u0018\u00010\u0001H\u00d6\u0003J\t\u0010\u001a\u001a\u00020\u001bH\u00d6\u0001J\t\u0010\u001c\u001a\u00020\u0005H\u00d6\u0001R\u001d\u0010\u0006\u001a\u000e\u0012\u0004\u0012\u00020\u0005\u0012\u0004\u0012\u00020\u00050\u0007\u00a2\u0006\b\n\u0000\u001a\u0004\b\u000b\u0010\fR\u0011\u0010\u0002\u001a\u00020\u0003\u00a2\u0006\b\n\u0000\u001a\u0004\b\r\u0010\u000eR\u0011\u0010\u0004\u001a\u00020\u0005\u00a2\u0006\b\n\u0000\u001a\u0004\b\u000f\u0010\u0010R#\u0010\b\u001a\u0014\u0012\u0004\u0012\u00020\u0005\u0012\n\u0012\b\u0012\u0004\u0012\u00020\u00050\t0\u0007\u00a2\u0006\b\n\u0000\u001a\u0004\b\u0011\u0010\f\u00a8\u0006\u001d"}, d2 = {"Lorg/openapitools/client/infrastructure/RequestConfig;", "", "method", "Lorg/openapitools/client/infrastructure/RequestMethod;", "path", "", "headers", "", "query", "", "(Lorg/openapitools/client/infrastructure/RequestMethod;Ljava/lang/String;Ljava/util/Map;Ljava/util/Map;)V", "getHeaders", "()Ljava/util/Map;", "getMethod", "()Lorg/openapitools/client/infrastructure/RequestMethod;", "getPath", "()Ljava/lang/String;", "getQuery", "component1", "component2", "component3", "component4", "copy", "equals", "", "other", "hashCode", "", "toString", "kotlin-petstore-moshi-codegen"})
public final class RequestConfig {
    @org.jetbrains.annotations.NotNull()
    private final org.openapitools.client.infrastructure.RequestMethod method = null;
    @org.jetbrains.annotations.NotNull()
    private final java.lang.String path = null;
    @org.jetbrains.annotations.NotNull()
    private final java.util.Map<java.lang.String, java.lang.String> headers = null;
    @org.jetbrains.annotations.NotNull()
    private final java.util.Map<java.lang.String, java.util.List<java.lang.String>> query = null;
    
    @org.jetbrains.annotations.NotNull()
    public final org.openapitools.client.infrastructure.RequestMethod getMethod() {
        return null;
    }
    
    @org.jetbrains.annotations.NotNull()
    public final java.lang.String getPath() {
        return null;
    }
    
    @org.jetbrains.annotations.NotNull()
    public final java.util.Map<java.lang.String, java.lang.String> getHeaders() {
        return null;
    }
    
    @org.jetbrains.annotations.NotNull()
    public final java.util.Map<java.lang.String, java.util.List<java.lang.String>> getQuery() {
        return null;
    }
    
    public RequestConfig(@org.jetbrains.annotations.NotNull()
    org.openapitools.client.infrastructure.RequestMethod method, @org.jetbrains.annotations.NotNull()
    java.lang.String path, @org.jetbrains.annotations.NotNull()
    java.util.Map<java.lang.String, java.lang.String> headers, @org.jetbrains.annotations.NotNull()
    java.util.Map<java.lang.String, java.util.List<java.lang.String>> query) {
        super();
    }
    
    @org.jetbrains.annotations.NotNull()
    public final org.openapitools.client.infrastructure.RequestMethod component1() {
        return null;
    }
    
    @org.jetbrains.annotations.NotNull()
    public final java.lang.String component2() {
        return null;
    }
    
    @org.jetbrains.annotations.NotNull()
    public final java.util.Map<java.lang.String, java.lang.String> component3() {
        return null;
    }
    
    @org.jetbrains.annotations.NotNull()
    public final java.util.Map<java.lang.String, java.util.List<java.lang.String>> component4() {
        return null;
    }
    
    /**
     * Defines a config object for a given request.
     * NOTE: This object doesn't include 'body' because it
     *      allows for caching of the constructed object
     *      for many request definitions.
     * NOTE: Headers is a Map<String,String> because rfc2616 defines
     *      multi-valued headers as csv-only.
     */
    @org.jetbrains.annotations.NotNull()
    public final org.openapitools.client.infrastructure.RequestConfig copy(@org.jetbrains.annotations.NotNull()
    org.openapitools.client.infrastructure.RequestMethod method, @org.jetbrains.annotations.NotNull()
    java.lang.String path, @org.jetbrains.annotations.NotNull()
    java.util.Map<java.lang.String, java.lang.String> headers, @org.jetbrains.annotations.NotNull()
    java.util.Map<java.lang.String, java.util.List<java.lang.String>> query) {
        return null;
    }
    
    /**
     * Defines a config object for a given request.
     * NOTE: This object doesn't include 'body' because it
     *      allows for caching of the constructed object
     *      for many request definitions.
     * NOTE: Headers is a Map<String,String> because rfc2616 defines
     *      multi-valued headers as csv-only.
     */
    @org.jetbrains.annotations.NotNull()
    @java.lang.Override()
    public java.lang.String toString() {
        return null;
    }
    
    /**
     * Defines a config object for a given request.
     * NOTE: This object doesn't include 'body' because it
     *      allows for caching of the constructed object
     *      for many request definitions.
     * NOTE: Headers is a Map<String,String> because rfc2616 defines
     *      multi-valued headers as csv-only.
     */
    @java.lang.Override()
    public int hashCode() {
        return 0;
    }
    
    /**
     * Defines a config object for a given request.
     * NOTE: This object doesn't include 'body' because it
     *      allows for caching of the constructed object
     *      for many request definitions.
     * NOTE: Headers is a Map<String,String> because rfc2616 defines
     *      multi-valued headers as csv-only.
     */
    @java.lang.Override()
    public boolean equals(@org.jetbrains.annotations.Nullable()
    java.lang.Object p0) {
        return false;
    }
}