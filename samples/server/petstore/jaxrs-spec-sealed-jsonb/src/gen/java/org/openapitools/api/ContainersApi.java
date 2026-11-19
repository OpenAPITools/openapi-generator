package org.openapitools.api;

import org.openapitools.model.Containers;

import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.io.InputStream;
import java.util.Map;
import java.util.List;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

/**
* Represents a collection of functions to interact with the API endpoints.
*/
@Path("/containers")
@Api(description = "the containers API")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.28.0-SNAPSHOT")
public class ContainersApi {

    @POST
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @ApiOperation(value = "", notes = "", response = Containers.class, tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "The containers that were read", response = Containers.class)
    })
    public Response postContainers(@Valid @NotNull Containers containers
) {
        return Response.ok().entity("magic!").build();
    }
}
