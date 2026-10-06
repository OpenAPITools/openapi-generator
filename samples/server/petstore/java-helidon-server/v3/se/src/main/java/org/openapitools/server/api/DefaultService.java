package org.openapitools.server.api;


import io.helidon.webserver.Routing;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;
import io.helidon.webserver.Service;

public interface DefaultService extends Service { 

    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    default void update(Routing.Rules rules) {
        rules.get("/foo", this::fooGet);
    }


    /**
     * GET /foo.
     * @param request the server request
     * @param response the server response
     */
    void fooGet(ServerRequest request, ServerResponse response);

}
