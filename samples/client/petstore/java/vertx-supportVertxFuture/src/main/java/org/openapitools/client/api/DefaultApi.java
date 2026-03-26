package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.model.FooGetDefaultResponse;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface DefaultApi {

    void fooGet(Handler<AsyncResult<FooGetDefaultResponse>> handler);

    default Future<FooGetDefaultResponse> fooGet(){
        Promise<FooGetDefaultResponse> promise = Promise.promise();
        fooGet(promise);
        return promise.future();
    }

    void fooGet(ApiClient.AuthInfo authInfo, Handler<AsyncResult<FooGetDefaultResponse>> handler);

    default Future<FooGetDefaultResponse> fooGet(ApiClient.AuthInfo authInfo){
        Promise<FooGetDefaultResponse> promise = Promise.promise();
        fooGet(authInfo, promise);
        return promise.future();
    }

}
