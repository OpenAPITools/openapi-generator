package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.model.Client;
import java.util.UUID;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface AnotherFakeApi {

    void call123testSpecialTags(@javax.annotation.Nonnull UUID uuidTest, @javax.annotation.Nonnull Client body, Handler<AsyncResult<Client>> handler);

    void call123testSpecialTags(@javax.annotation.Nonnull UUID uuidTest, @javax.annotation.Nonnull Client body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Client>> handler);

}
