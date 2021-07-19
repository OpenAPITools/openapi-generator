package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.model.InlineResponseDefault;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface DefaultApi {

    void fooGet(Handler<AsyncResult<InlineResponseDefault>> handler);

    void fooGet(ApiClient.AuthInfo authInfo, Handler<AsyncResult<InlineResponseDefault>> handler);

}
