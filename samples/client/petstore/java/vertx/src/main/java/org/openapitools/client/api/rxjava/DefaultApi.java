package org.openapitools.client.api.rxjava;

import org.openapitools.client.model.XmlItem;

import java.util.*;

import rx.Single;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;


public class DefaultApi {

	private final org.openapitools.client.api.DefaultApi delegate;

	public DefaultApi(org.openapitools.client.api.DefaultApi delegate) {
	    this.delegate = delegate;
    }

	public org.openapitools.client.api.DefaultApi getDelegate() {
	    return delegate;
	}

    /**
     * creates an XmlItem
     * this route creates an XmlItem
     * @param xmlItem XmlItem Body (required)
     * @param resultHandler Asynchronous result handler
     */
    public void createXmlItem(XmlItem xmlItem, Handler<AsyncResult<Void>> resultHandler) {
        delegate.createXmlItem(xmlItem, resultHandler);
    }

    /**
     * creates an XmlItem
     * this route creates an XmlItem
     * @param xmlItem XmlItem Body (required)
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxCreateXmlItem(XmlItem xmlItem) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.createXmlItem(xmlItem, fut);
        }));
    }

    public static DefaultApi newInstance(org.openapitools.client.api.DefaultApi arg) {
        return arg != null ? new DefaultApi(arg) : null;
    }
}
