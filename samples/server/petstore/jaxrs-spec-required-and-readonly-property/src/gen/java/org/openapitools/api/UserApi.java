package org.openapitools.api;

import org.openapitools.model.ReadonlyAndRequiredProperties;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/user")
@Api(description = "the user API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
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
