package org.openapitools.server.api;

import org.openapitools.server.model.Client;
import io.helidon.webserver.Handler;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.helidon.webserver.Routing;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;
import io.helidon.webserver.Service;

public interface AnotherFakeService extends Service { 

    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    default void update(Routing.Rules rules) {
        rules.patch("/another-fake/dummy", Handler.create(Client.class, this::call123testSpecialTags));
    }


    /**
     * PATCH /another-fake/dummy : To test special tags.
     * @param request the server request
     * @param response the server response
     * @param client client model 
     */
    void call123testSpecialTags(ServerRequest request, ServerResponse response, Client client);

}
