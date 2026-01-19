package org.openapitools.api;

import org.openapitools.model.FooGetDefaultResponse;

import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Response;

import io.swagger.v3.oas.annotations.*;
import io.swagger.v3.oas.annotations.media.*;
import io.swagger.v3.oas.annotations.responses.*;
import io.swagger.v3.oas.annotations.tags.Tag;

import java.io.InputStream;
import java.util.Map;
import java.util.List;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

/**
* Represents a collection of functions to interact with the API endpoints.
*/
@Path("/foo")
@Tag(name = "foo")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.20.0-SNAPSHOT")
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
