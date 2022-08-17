package org.openapitools.api;

import org.openapitools.model.Client;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;


import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/another-fake/dummy")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")public interface AnotherFakeApi {

    @PATCH
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    Client call123testSpecialTags(@Valid @NotNull Client body);
}
