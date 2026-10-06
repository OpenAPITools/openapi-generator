package org.openapitools.api;


import jakarta.ws.rs.*;
import org.jboss.resteasy.reactive.ResponseStatus;

import jakarta.validation.constraints.*;

@Path("/admin")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public interface AdminApi {

    @GET
    @ResponseStatus(200)
    @jakarta.annotation.security.RolesAllowed({"admin"})
    void getAdmin();

}
