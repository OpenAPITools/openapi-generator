package io.swagger.client.api;

import io.swagger.client.model.Client;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface FakeClassnameTags123Api {

    void testClassname(Client body, Handler<AsyncResult<Client>> handler);

}
