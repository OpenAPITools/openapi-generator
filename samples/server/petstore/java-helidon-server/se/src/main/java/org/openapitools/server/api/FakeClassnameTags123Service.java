package org.openapitools.server.api;

import org.openapitools.server.model.Client;
import io.helidon.webserver.Handler;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.helidon.webserver.Routing;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;
import io.helidon.webserver.Service;

public interface FakeClassnameTags123Service extends Service { 

    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    default void update(Routing.Rules rules) {
        rules.patch("/fake_classname_test", Handler.create(Client.class, this::testClassname));
    }


    /**
     * PATCH /fake_classname_test : To test class name in snake case.
     * @param request the server request
     * @param response the server response
     * @param client client model 
     */
    void testClassname(ServerRequest request, ServerResponse response, Client client);

}
