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
                             trigger = "tag = 'FakeClassnameTags123'",
                             version = "stable")
public interface FakeClassnameTags123Service extends HttpService {

    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    default void routing(HttpRules rules) {
        rules.patch("/", this::testClassname);
    }


    /**
     * PATCH /fake_classname_test : To test class name in snake case.
     *
     * @param request the server request
     * @param response the server response
     */
    void testClassname(ServerRequest request, ServerResponse response);
}
