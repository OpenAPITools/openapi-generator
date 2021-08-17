package org.openapitools.api;

import org.openapitools.model.Client;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/fake_classname_test")
@Api(description = "the fake_classname_test API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")public class FakeClassnameTestApi {

    @PATCH
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @ApiOperation(value = "To test class name in snake case", notes = "To test class name in snake case", response = Client.class, authorizations = {
        
        @Authorization(value = "api_key_query")
         }, tags={ "fake_classname_tags 123#$%^" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class)
    })
    public Response testClassname(@Valid @NotNull Client body) {
        return Response.ok().entity("magic!").build();
    }
}
