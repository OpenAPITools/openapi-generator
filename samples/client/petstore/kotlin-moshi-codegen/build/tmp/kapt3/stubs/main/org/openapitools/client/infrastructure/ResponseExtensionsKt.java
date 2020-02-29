package org.openapitools.client.infrastructure;

import java.lang.System;

@kotlin.Metadata(mv = {1, 1, 16}, bv = {1, 0, 3}, k = 2, d1 = {"\u0000\u000e\n\u0000\n\u0002\u0010\u000b\n\u0002\u0018\u0002\n\u0002\b\u0005\"\u0015\u0010\u0000\u001a\u00020\u0001*\u00020\u00028F\u00a2\u0006\u0006\u001a\u0004\b\u0000\u0010\u0003\"\u0015\u0010\u0004\u001a\u00020\u0001*\u00020\u00028F\u00a2\u0006\u0006\u001a\u0004\b\u0004\u0010\u0003\"\u0015\u0010\u0005\u001a\u00020\u0001*\u00020\u00028F\u00a2\u0006\u0006\u001a\u0004\b\u0005\u0010\u0003\"\u0015\u0010\u0006\u001a\u00020\u0001*\u00020\u00028F\u00a2\u0006\u0006\u001a\u0004\b\u0006\u0010\u0003\u00a8\u0006\u0007"}, d2 = {"isClientError", "", "Lokhttp3/Response;", "(Lokhttp3/Response;)Z", "isInformational", "isRedirect", "isServerError", "kotlin-petstore-moshi-codegen"})
public final class ResponseExtensionsKt {
    
    public static final boolean isInformational(@org.jetbrains.annotations.NotNull()
    okhttp3.Response $this$isInformational) {
        return false;
    }
    
    public static final boolean isRedirect(@org.jetbrains.annotations.NotNull()
    okhttp3.Response $this$isRedirect) {
        return false;
    }
    
    public static final boolean isClientError(@org.jetbrains.annotations.NotNull()
    okhttp3.Response $this$isClientError) {
        return false;
    }
    
    public static final boolean isServerError(@org.jetbrains.annotations.NotNull()
    okhttp3.Response $this$isServerError) {
        return false;
    }
}