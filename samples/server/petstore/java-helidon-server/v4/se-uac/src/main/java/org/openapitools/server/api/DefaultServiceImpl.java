package org.openapitools.server.api;

import org.openapitools.server.model.FooGetDefaultResponse;
import java.util.HexFormat;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.helidon.http.Status;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;

public class DefaultServiceImpl extends DefaultService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;

    @Override
    protected void handleFooGet(ServerRequest request, ServerResponse response) {


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

}
