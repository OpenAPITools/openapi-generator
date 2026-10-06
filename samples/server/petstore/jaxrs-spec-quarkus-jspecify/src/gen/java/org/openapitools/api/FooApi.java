package org.openapitools.api;

import org.openapitools.model.Foo;
import org.jspecify.annotations.Nullable;
import java.time.OffsetDateTime;

import jakarta.ws.rs.*;

import jakarta.validation.constraints.*;

@Path("/foo/{dtParam}")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public interface FooApi {

    @GET
    @Produces({ "application/json" })
    Foo fooDtParamGet(@PathParam("dtParam") @Nullable OffsetDateTime dtParam
,@QueryParam("dtQuery")   @Nullable OffsetDateTime dtQuery
,@CookieParam("dtCookie")   @Nullable OffsetDateTime dtCookie
,@QueryParam("color") @DefaultValue("red")   @Nullable String color
);

}
