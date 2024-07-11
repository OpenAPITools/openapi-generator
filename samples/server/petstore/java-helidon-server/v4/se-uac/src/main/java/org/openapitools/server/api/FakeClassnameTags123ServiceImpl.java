package org.openapitools.server.api;

import org.openapitools.server.model.Client;
import java.util.HexFormat;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.helidon.http.Status;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;

public class FakeClassnameTags123ServiceImpl extends FakeClassnameTags123Service {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;

    @Override
    protected void handleTestClassname(ServerRequest request, ServerResponse response, 
                Client client) {


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

}
