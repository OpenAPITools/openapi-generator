package org.openapitools.api;


import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Response;
import org.jboss.resteasy.reactive.ResponseStatus;



import java.io.InputStream;
import java.util.Map;
import java.util.List;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;


@Path("/anonymous-or-authenticated")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.24.0-SNAPSHOT")
public interface AnonymousOrAuthenticatedApi {

    @GET
    @ResponseStatus(200)
    @jakarta.annotation.security.PermitAll
    void getAnonymousOrAuthenticated();

}
