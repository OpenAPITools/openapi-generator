package org.openapitools.client.api;

import org.openapitools.client.model.Client;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface AnotherFakeApi {

    void call123testSpecialTags(Client body, Handler<AsyncResult<Client>> handler);

}
