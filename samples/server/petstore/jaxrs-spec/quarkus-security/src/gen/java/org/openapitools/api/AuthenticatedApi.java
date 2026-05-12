package org.openapitools.api;


import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Response;
import org.jboss.resteasy.reactive.ResponseStatus;



import java.io.InputStream;
import java.util.Map;
import java.util.List;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;


@Path("/authenticated")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.23.0-SNAPSHOT")
public interface AuthenticatedApi {

    @GET
    @ResponseStatus(200)
    @jakarta.annotation.security.RolesAllowed({"**"})
    void getAuthenticated();

}
