package org.openapitools.api;

import org.openapitools.model.FooGetDefaultResponse;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

/**
* Represents a collection of functions to interact with the API endpoints.
*/
@Path("/foo")
@Api(description = "the foo API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class FooApi {

    @GET
    @Produces({ "application/json" })
    @ApiOperation(value = "", notes = "", response = FooGetDefaultResponse.class, tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "response", response = FooGetDefaultResponse.class)
    })
    public Response fooGet() {
        return Response.ok().entity("magic!").build();
    }
}
