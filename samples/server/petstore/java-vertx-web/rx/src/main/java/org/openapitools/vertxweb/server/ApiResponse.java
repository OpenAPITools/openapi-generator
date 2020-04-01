package org.openapitools.vertxweb.server;

public class ApiResponse<T> {
    private final T data;
    private Integer statusCode;

    public ApiResponse(T data) {
        this.data = data;
    }

    public ApiResponse() {
        this.data = null;
    }

    public T getData() {
        return data;
    }

    public ApiResponse<T> setStatusCode(Integer statusCode) {
        this.statusCode = statusCode;
        return this;
    }

    public Integer getStatusCode() {
        return statusCode;
    }
}
    

