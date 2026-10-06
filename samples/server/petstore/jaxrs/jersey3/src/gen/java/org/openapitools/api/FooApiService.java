package org.openapitools.api;

import org.openapitools.api.*;

import org.openapitools.api.NotFoundException;

import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.SecurityContext;
import jakarta.validation.constraints.*;
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public abstract class FooApiService {
    public abstract Response fooGet(SecurityContext securityContext) throws NotFoundException;
}
