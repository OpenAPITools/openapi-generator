package io.swagger.client.api.rxjava;

import io.swagger.client.model.Client;

import java.util.*;

import rx.Single;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;


public class FakeClassnameTags123Api {

	private final io.swagger.client.api.FakeClassnameTags123Api delegate;

	public FakeClassnameTags123Api(io.swagger.client.api.FakeClassnameTags123Api delegate) {
	    this.delegate = delegate;
    }

	public io.swagger.client.api.FakeClassnameTags123Api getDelegate() {
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
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Client> rxTestClassname(Client client) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.testClassname(client, fut);
        }));
    }

    public static FakeClassnameTags123Api newInstance(io.swagger.client.api.FakeClassnameTags123Api arg) {
        return arg != null ? new FakeClassnameTags123Api(arg) : null;
    }
}
