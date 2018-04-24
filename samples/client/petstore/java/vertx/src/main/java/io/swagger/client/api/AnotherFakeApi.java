package io.swagger.client.api;

import io.swagger.client.model.Client;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface AnotherFakeApi {

    void testSpecialTags(Client client, Handler<AsyncResult<Client>> handler);

}
