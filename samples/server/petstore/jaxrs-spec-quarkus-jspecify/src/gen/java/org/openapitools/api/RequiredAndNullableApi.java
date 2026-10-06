package org.openapitools.api;

import org.openapitools.model.RequiredAndNullable;

import jakarta.ws.rs.*;

import jakarta.validation.constraints.*;
import jakarta.validation.Valid;


@Path("/requiredAndNullable")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public interface RequiredAndNullableApi {

    @POST
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    RequiredAndNullable requiredAndNullablePost(@Valid @NotNull RequiredAndNullable requiredAndNullable
);

}
