package org.openapitools.client.infrastructure;

import java.lang.System;

@kotlin.Metadata(mv = {1, 1, 16}, bv = {1, 0, 3}, k = 1, d1 = {"\u0000\u0018\n\u0002\u0018\u0002\n\u0002\u0018\u0002\n\u0000\n\u0002\u0010\u000e\n\u0000\n\u0002\u0010\b\n\u0002\b\u0005\b\u0016\u0018\u0000 \t2\u00020\u0001:\u0001\tB\u001b\u0012\n\b\u0002\u0010\u0002\u001a\u0004\u0018\u00010\u0003\u0012\b\b\u0002\u0010\u0004\u001a\u00020\u0005\u00a2\u0006\u0002\u0010\u0006R\u0011\u0010\u0004\u001a\u00020\u0005\u00a2\u0006\b\n\u0000\u001a\u0004\b\u0007\u0010\b\u00a8\u0006\n"}, d2 = {"Lorg/openapitools/client/infrastructure/ServerException;", "Ljava/lang/RuntimeException;", "message", "", "statusCode", "", "(Ljava/lang/String;I)V", "getStatusCode", "()I", "Companion", "kotlin-petstore-moshi-codegen"})
public class ServerException extends java.lang.RuntimeException {
    private final int statusCode = 0;
    private static final long serialVersionUID = 456L;
    public static final org.openapitools.client.infrastructure.ServerException.Companion Companion = null;
    
    public final int getStatusCode() {
        return 0;
    }
    
    public ServerException(@org.jetbrains.annotations.Nullable()
    java.lang.String message, int statusCode) {
        super();
    }
    
    public ServerException() {
        super();
    }
    
    @kotlin.Metadata(mv = {1, 1, 16}, bv = {1, 0, 3}, k = 1, d1 = {"\u0000\u0012\n\u0002\u0018\u0002\n\u0002\u0010\u0000\n\u0002\b\u0002\n\u0002\u0010\t\n\u0000\b\u0086\u0003\u0018\u00002\u00020\u0001B\u0007\b\u0002\u00a2\u0006\u0002\u0010\u0002R\u000e\u0010\u0003\u001a\u00020\u0004X\u0082T\u00a2\u0006\u0002\n\u0000\u00a8\u0006\u0005"}, d2 = {"Lorg/openapitools/client/infrastructure/ServerException$Companion;", "", "()V", "serialVersionUID", "", "kotlin-petstore-moshi-codegen"})
    public static final class Companion {
        
        private Companion() {
            super();
        }
    }
}