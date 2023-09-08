package org.openapitools.client.api.rxjava;

import org.openapitools.client.model.FooGetDefaultResponse;
import org.openapitools.client.ApiClient;

import java.util.*;

import rx.Single;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class DefaultApi {

    private final org.openapitools.client.api.DefaultApi delegate;

    public DefaultApi(org.openapitools.client.api.DefaultApi delegate) {
        this.delegate = delegate;
    }

    public org.openapitools.client.api.DefaultApi getDelegate() {
        return delegate;
    }

    /**
    * 
    * 
    * @param resultHandler Asynchronous result handler
    */
    public void fooGet(Handler<AsyncResult<FooGetDefaultResponse>> resultHandler) {
        delegate.fooGet(resultHandler);
    }

    /**
    * 
    * 
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void fooGet(ApiClient.AuthInfo authInfo, Handler<AsyncResult<FooGetDefaultResponse>> resultHandler) {
        delegate.fooGet(authInfo, resultHandler);
    }

    /**
    * 
    * 
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<FooGetDefaultResponse> rxFooGet() {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.fooGet(fut)
        ));
    }

    /**
    * 
    * 
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<FooGetDefaultResponse> rxFooGet(ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.fooGet(authInfo, fut)
        ));
    }

    public static DefaultApi newInstance(org.openapitools.client.api.DefaultApi arg) {
        return arg != null ? new DefaultApi(arg) : null;
    }
}
