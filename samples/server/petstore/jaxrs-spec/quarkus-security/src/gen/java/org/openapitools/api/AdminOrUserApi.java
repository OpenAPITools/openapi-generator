package org.openapitools.api;


import jakarta.ws.rs.*;
import org.jboss.resteasy.reactive.ResponseStatus;

import jakarta.validation.constraints.*;

@Path("/admin-or-user")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public interface AdminOrUserApi {

    @GET
    @ResponseStatus(200)
    @jakarta.annotation.security.RolesAllowed({"admin","user"})
    void getAdminOrUser();

}
