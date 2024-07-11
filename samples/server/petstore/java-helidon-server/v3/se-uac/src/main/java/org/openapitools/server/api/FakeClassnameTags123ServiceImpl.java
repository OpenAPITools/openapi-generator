package org.openapitools.server.api;

import org.openapitools.server.model.Client;
import io.helidon.webserver.Handler;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;

public class FakeClassnameTags123ServiceImpl extends FakeClassnameTags123Service {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;

    public void handleTestClassname(ServerRequest request, ServerResponse response, Client client) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }


    public Void handleError(ServerRequest request, ServerResponse response, Throwable throwable) {
        return response.send(throwable);
    }
}
