package org.openapitools.api.impl;

import org.openapitools.api.*;
import org.openapitools.model.*;

import org.openapitools.model.Client;
import java.util.UUID;

import org.openapitools.api.NotFoundException;

import javax.ws.rs.core.Response;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaMSF4JServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public class AnotherFakeApiServiceImpl extends AnotherFakeApiService {
    @Override
    public Response call123testSpecialTags(UUID uuidTest
, Client body
 ) throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }
}
