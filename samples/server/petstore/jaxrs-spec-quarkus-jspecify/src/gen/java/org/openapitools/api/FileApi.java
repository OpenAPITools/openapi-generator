package org.openapitools.api;

import org.openapitools.model.FileContent;

import jakarta.ws.rs.*;
import org.jboss.resteasy.reactive.ResponseStatus;

import jakarta.validation.constraints.*;

@Path("/file/{id}")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public interface FileApi {

    @GET
    @Produces({ "application/json" })
    @ResponseStatus(200)
    FileContent fileIdGet(@PathParam("id") String id
);

}
