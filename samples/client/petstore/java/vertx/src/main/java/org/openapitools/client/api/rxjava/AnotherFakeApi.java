package org.openapitools.client.api.rxjava;

import org.openapitools.client.model.Client;

import java.util.*;

import rx.Single;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;


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
     * To test special tags
     * @param client client model (required)
     * @param resultHandler Asynchronous result handler
     */
    public void testSpecialTags(Client client, Handler<AsyncResult<Client>> resultHandler) {
        delegate.testSpecialTags(client, resultHandler);
    }

    /**
     * To test special tags
     * To test special tags
     * @param client client model (required)
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Client> rxTestSpecialTags(Client client) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.testSpecialTags(client, fut);
        }));
    }

    public static AnotherFakeApi newInstance(org.openapitools.client.api.AnotherFakeApi arg) {
        return arg != null ? new AnotherFakeApi(arg) : null;
    }
}
