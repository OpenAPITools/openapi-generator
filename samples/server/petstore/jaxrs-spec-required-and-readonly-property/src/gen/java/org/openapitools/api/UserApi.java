package org.openapitools.api;

import org.openapitools.model.ReadonlyAndRequiredProperties;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import javax.validation.constraints.*;

/**
* Represents a collection of functions to interact with the API endpoints.
*/
@Path("/user")
@Api(description = "the user API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public class UserApi {

    @GET
    @Produces({ "application/json" })
    @ApiOperation(value = "", notes = "", response = ReadonlyAndRequiredProperties.class, tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "success", response = ReadonlyAndRequiredProperties.class)
    })
    public Response userGet() {
        return Response.ok().entity("magic!").build();
    }
}
