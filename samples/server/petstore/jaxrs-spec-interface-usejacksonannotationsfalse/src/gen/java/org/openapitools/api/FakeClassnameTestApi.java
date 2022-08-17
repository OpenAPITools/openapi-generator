package org.openapitools.api;

import org.openapitools.model.Client;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;


import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/fake_classname_test")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")public interface FakeClassnameTestApi {

    @PATCH
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    Client testClassname(@Valid @NotNull Client body);
}
