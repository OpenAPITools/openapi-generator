package org.openapitools.client.infrastructure;

import java.lang.System;

@kotlin.Metadata(mv = {1, 1, 16}, bv = {1, 0, 3}, k = 1, d1 = {"\u0000(\n\u0002\u0018\u0002\n\u0000\n\u0002\u0018\u0002\n\u0000\n\u0002\u0010\u0000\n\u0000\n\u0002\u0010\b\n\u0000\n\u0002\u0010$\n\u0002\u0010\u000e\n\u0002\u0010 \n\u0002\b\b\u0018\u0000*\u0004\b\u0000\u0010\u00012\b\u0012\u0004\u0012\u0002H\u00010\u0002B7\u0012\n\b\u0002\u0010\u0003\u001a\u0004\u0018\u00010\u0004\u0012\b\b\u0002\u0010\u0005\u001a\u00020\u0006\u0012\u001a\b\u0002\u0010\u0007\u001a\u0014\u0012\u0004\u0012\u00020\t\u0012\n\u0012\b\u0012\u0004\u0012\u00020\t0\n0\b\u00a2\u0006\u0002\u0010\u000bR\u0013\u0010\u0003\u001a\u0004\u0018\u00010\u0004\u00a2\u0006\b\n\u0000\u001a\u0004\b\f\u0010\rR&\u0010\u0007\u001a\u0014\u0012\u0004\u0012\u00020\t\u0012\n\u0012\b\u0012\u0004\u0012\u00020\t0\n0\bX\u0096\u0004\u00a2\u0006\b\n\u0000\u001a\u0004\b\u000e\u0010\u000fR\u0014\u0010\u0005\u001a\u00020\u0006X\u0096\u0004\u00a2\u0006\b\n\u0000\u001a\u0004\b\u0010\u0010\u0011\u00a8\u0006\u0012"}, d2 = {"Lorg/openapitools/client/infrastructure/ClientError;", "T", "Lorg/openapitools/client/infrastructure/ApiInfrastructureResponse;", "body", "", "statusCode", "", "headers", "", "", "", "(Ljava/lang/Object;ILjava/util/Map;)V", "getBody", "()Ljava/lang/Object;", "getHeaders", "()Ljava/util/Map;", "getStatusCode", "()I", "kotlin-petstore-moshi-codegen"})
public final class ClientError<T extends java.lang.Object> extends org.openapitools.client.infrastructure.ApiInfrastructureResponse<T> {
    @org.jetbrains.annotations.Nullable()
    private final java.lang.Object body = null;
    private final int statusCode = 0;
    @org.jetbrains.annotations.NotNull()
    private final java.util.Map<java.lang.String, java.util.List<java.lang.String>> headers = null;
    
    @org.jetbrains.annotations.Nullable()
    public final java.lang.Object getBody() {
        return null;
    }
    
    @java.lang.Override()
    public int getStatusCode() {
        return 0;
    }
    
    @org.jetbrains.annotations.NotNull()
    @java.lang.Override()
    public java.util.Map<java.lang.String, java.util.List<java.lang.String>> getHeaders() {
        return null;
    }
    
    public ClientError(@org.jetbrains.annotations.Nullable()
    java.lang.Object body, int statusCode, @org.jetbrains.annotations.NotNull()
    java.util.Map<java.lang.String, ? extends java.util.List<java.lang.String>> headers) {
        super(null);
    }
    
    public ClientError() {
        super(null);
    }
}