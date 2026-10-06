package org.openapitools.api;


import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Response;

import io.swagger.v3.oas.annotations.*;
import io.swagger.v3.oas.annotations.media.*;
import io.swagger.v3.oas.annotations.responses.*;
import io.swagger.v3.oas.annotations.tags.Tag;

import jakarta.validation.constraints.*;

/**
* Represents a collection of functions to interact with the API endpoints.
*/
@Path("/foo")
@Tag(name = "foo")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public class FooApi {

    @GET
    @Produces({ "application/json" })
    @Operation(summary = "", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "response")
    })
    public Response fooGet() {
        return Response.ok().entity("magic!").build();
    }
}
