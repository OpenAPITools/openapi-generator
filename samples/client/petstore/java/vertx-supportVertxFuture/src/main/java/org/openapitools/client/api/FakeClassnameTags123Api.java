package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.model.Client;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface FakeClassnameTags123Api {

    void testClassname(@javax.annotation.Nonnull Client client, Handler<AsyncResult<Client>> handler);

    default Future<Client> testClassname(@javax.annotation.Nonnull Client client){
        Promise<Client> promise = Promise.promise();
        testClassname(client, promise);
        return promise.future();
    }

    void testClassname(@javax.annotation.Nonnull Client client, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Client>> handler);

    default Future<Client> testClassname(@javax.annotation.Nonnull Client client, ApiClient.AuthInfo authInfo){
        Promise<Client> promise = Promise.promise();
        testClassname(client, authInfo, promise);
        return promise.future();
    }

}
