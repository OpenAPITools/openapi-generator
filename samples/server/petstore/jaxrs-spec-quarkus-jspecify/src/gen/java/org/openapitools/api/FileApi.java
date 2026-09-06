package org.openapitools.api;

import org.openapitools.model.FileContent;

import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Response;
import org.jboss.resteasy.reactive.ResponseStatus;



import java.io.InputStream;
import java.util.Map;
import java.util.List;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;


@Path("/file/{id}")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public interface FileApi {

    @GET
    @Produces({ "application/json" })
    @ResponseStatus(200)
    FileContent fileIdGet(@PathParam("id") String id);

}
