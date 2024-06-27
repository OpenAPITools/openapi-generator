package org.openapitools.server.api;

import org.openapitools.server.model.FooGetDefaultResponse;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.helidon.http.Status;

import io.helidon.webserver.http.HttpRules;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import io.helidon.webserver.http.HttpService;

public interface DefaultService extends HttpService { 

    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    default void routing(HttpRules rules) {
        rules.get("/foo", this::fooGet);
    }


    /**
     * GET /foo.
     *
     * @param request the server request
     * @param response the server response
     */
    void fooGet(ServerRequest request, ServerResponse response);


}
