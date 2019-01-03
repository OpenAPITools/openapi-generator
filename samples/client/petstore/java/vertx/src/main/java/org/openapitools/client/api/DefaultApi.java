package org.openapitools.client.api;

import org.openapitools.client.model.XmlItem;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface DefaultApi {

    void createXmlItem(XmlItem xmlItem, Handler<AsyncResult<Void>> handler);

}
