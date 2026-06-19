package org.openapitools.server.api;

import org.openapitools.server.model.FooGetDefaultResponse;
import java.util.HexFormat;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.helidon.http.Status;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;

public class DefaultServiceImpl extends DefaultService {

    @Override
    protected void handleFooGet(ServerRequest request, ServerResponse response) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

}
