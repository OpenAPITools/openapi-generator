package org.openapitools.api;


import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Response;



import java.io.InputStream;
import java.util.Map;
import java.util.List;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;


@Path("/ping")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.28.0-SNAPSHOT")
public class PingApi {

    @io.quarkus.vertx.http.Compressed
    @GET
    public Response pingGet() {
        return Response.ok().entity("magic!").build();
    }

}
