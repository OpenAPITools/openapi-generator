package org.openapitools.server.api;

import org.openapitools.server.model.FooGetDefaultResponse;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;

public class DefaultServiceImpl extends DefaultService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;

    public void handleFooGet(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }


    public Void handleError(ServerRequest request, ServerResponse response, Throwable throwable) {
        return response.send(throwable);
    }
}
