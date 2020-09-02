package org.openapitools.client.api.rxjava;

import org.openapitools.client.model.Client;
import org.openapitools.client.ApiClient;

import java.util.*;

import rx.Single;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class FakeClassnameTags123Api {

    private final org.openapitools.client.api.FakeClassnameTags123Api delegate;

    public FakeClassnameTags123Api(org.openapitools.client.api.FakeClassnameTags123Api delegate) {
        this.delegate = delegate;
    }

    public org.openapitools.client.api.FakeClassnameTags123Api getDelegate() {
        return delegate;
    }

    /**
    * To test class name in snake case
    * To test class name in snake case
    * @param body client model (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testClassname(Client body, Handler<AsyncResult<Client>> resultHandler) {
        delegate.testClassname(body, resultHandler);
    }

    /**
    * To test class name in snake case
    * To test class name in snake case
    * @param body client model (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void testClassname(Client body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Client>> resultHandler) {
        delegate.testClassname(body, authInfo, resultHandler);
    }

    /**
    * To test class name in snake case
    * To test class name in snake case
    * @param body client model (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Client> rxTestClassname(Client body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testClassname(body, fut)
        ));
    }

    /**
    * To test class name in snake case
    * To test class name in snake case
    * @param body client model (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Client> rxTestClassname(Client body, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testClassname(body, authInfo, fut)
        ));
    }

    public static FakeClassnameTags123Api newInstance(org.openapitools.client.api.FakeClassnameTags123Api arg) {
        return arg != null ? new FakeClassnameTags123Api(arg) : null;
    }
}
