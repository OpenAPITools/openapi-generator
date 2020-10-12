package org.openapitools.client.api.rxjava;

import org.openapitools.client.model.Client;
import org.openapitools.client.ApiClient;

import java.util.*;

import rx.Single;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class AnotherFakeApi {

    private final org.openapitools.client.api.AnotherFakeApi delegate;

    public AnotherFakeApi(org.openapitools.client.api.AnotherFakeApi delegate) {
        this.delegate = delegate;
    }

    public org.openapitools.client.api.AnotherFakeApi getDelegate() {
        return delegate;
    }

    /**
    * To test special tags
    * To test special tags and operation ID starting with number
    * @param body client model (required)
    * @param resultHandler Asynchronous result handler
    */
    public void call123testSpecialTags(Client body, Handler<AsyncResult<Client>> resultHandler) {
        delegate.call123testSpecialTags(body, resultHandler);
    }

    /**
    * To test special tags
    * To test special tags and operation ID starting with number
    * @param body client model (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void call123testSpecialTags(Client body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Client>> resultHandler) {
        delegate.call123testSpecialTags(body, authInfo, resultHandler);
    }

    /**
    * To test special tags
    * To test special tags and operation ID starting with number
    * @param body client model (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Client> rxCall123testSpecialTags(Client body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.call123testSpecialTags(body, fut)
        ));
    }

    /**
    * To test special tags
    * To test special tags and operation ID starting with number
    * @param body client model (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Client> rxCall123testSpecialTags(Client body, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.call123testSpecialTags(body, authInfo, fut)
        ));
    }

    public static AnotherFakeApi newInstance(org.openapitools.client.api.AnotherFakeApi arg) {
        return arg != null ? new AnotherFakeApi(arg) : null;
    }
}
