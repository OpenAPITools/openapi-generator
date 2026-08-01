package org.openapitools.api;

import org.openapitools.model.PetRequest;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;


import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

/**
* Represents a collection of functions to interact with the API endpoints.
*/
@Path("/pets")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.25.0-SNAPSHOT")
public class PetsApi {

    @POST
    @Consumes({ "application/json" })
    public Response createPet(@Valid @NotNull PetRequest petRequest) {
        return Response.ok().entity("magic!").build();
    }
}
