package org.openapitools.api;

import org.openapitools.model.FooGetDefaultResponse;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import javax.validation.constraints.*;

/**
* Represents a collection of functions to interact with the API endpoints.
*/
@Path("/foo")
@Api(description = "the foo API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
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
