package org.openapitools.api;

import org.openapitools.model.Client;

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
@Path("/fake_classname_test")
@Tag(name = "fake_classname_test")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.20.0-SNAPSHOT")
public class FakeClassnameTestApi {

    @PATCH
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @Operation(summary = "To test class name in snake case", description = "To test class name in snake case")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation")
    })
    public Response testClassname(@Valid @NotNull Client client) {
        return Response.ok().entity("magic!").build();
    }
}
