package org.openapitools.server.api;

import org.openapitools.server.model.Client;
import java.util.HexFormat;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.helidon.http.Status;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;

public class AnotherFakeServiceImpl extends AnotherFakeService {

    @Override
    protected void handleCall123testSpecialTags(ServerRequest request, ServerResponse response, 
                Client client) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

}
