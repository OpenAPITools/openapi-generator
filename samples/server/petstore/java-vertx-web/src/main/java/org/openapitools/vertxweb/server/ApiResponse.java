package org.openapitools.vertxweb.server;
import io.vertx.ext.web.RoutingContext;

public class ApiResponse<T> {
    private final T data;
    private final int statusCode;

    public ApiResponse() {
        this(200, null);
    }

    public ApiResponse(T data) {
        this(200, data);
    }

    public ApiResponse(int statusCode) {
        this(statusCode, null);
    }

    public ApiResponse(int statusCode, T data) {
        this.statusCode = statusCode;
        this.data = data;
    }

    public boolean hasData() {
        return data != null;
    }

    public T getData() {
        return data;
    }

    public int getStatusCode() {
        return statusCode;
    }

    public void sendResponse(RoutingContext routingContext){
        if (this.hasData()) {
            routingContext.json(this.getData());
        } else {
            routingContext.response().end();
        }
    }
}
