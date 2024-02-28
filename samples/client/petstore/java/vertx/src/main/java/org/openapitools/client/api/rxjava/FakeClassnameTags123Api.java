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
    * @param client client model (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testClassname(Client client, Handler<AsyncResult<Client>> resultHandler) {
        delegate.testClassname(client, resultHandler);
    }

    /**
    * To test class name in snake case
    * To test class name in snake case
    * @param client client model (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void testClassname(Client client, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Client>> resultHandler) {
        delegate.testClassname(client, authInfo, resultHandler);
    }

    /**
    * To test class name in snake case
    * To test class name in snake case
    * @param client client model (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Client> rxTestClassname(Client client) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testClassname(client, fut)
        ));
    }

    /**
    * To test class name in snake case
    * To test class name in snake case
    * @param client client model (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Client> rxTestClassname(Client client, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.testClassname(client, authInfo, fut)
        ));
    }

    public static FakeClassnameTags123Api newInstance(org.openapitools.client.api.FakeClassnameTags123Api arg) {
        return arg != null ? new FakeClassnameTags123Api(arg) : null;
    }
}
