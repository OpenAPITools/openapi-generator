package org.openapitools.server.api;

import org.openapitools.server.model.Client;
import java.util.HexFormat;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.helidon.http.Status;

import io.helidon.webserver.http.HttpRules;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import io.helidon.webserver.http.HttpService;

@io.helidon.common.Generated(value = "org.openapitools.codegen.languages.JavaHelidonServerCodegen",
                             trigger = "tag = 'AnotherFake'",
                             version = "stable")
public interface AnotherFakeService extends HttpService {

    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    default void routing(HttpRules rules) {
        rules.patch("/", this::call123testSpecialTags);
    }


    /**
     * PATCH /another-fake/dummy : To test special tags.
     *
     * @param request the server request
     * @param response the server response
     */
    void call123testSpecialTags(ServerRequest request, ServerResponse response);
}
