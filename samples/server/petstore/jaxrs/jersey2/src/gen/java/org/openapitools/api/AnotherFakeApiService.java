package org.openapitools.api;

import org.openapitools.api.*;

import org.openapitools.model.Client;
import java.util.UUID;

import org.openapitools.api.NotFoundException;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.validation.constraints.*;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public abstract class AnotherFakeApiService {
    public abstract Response call123testSpecialTags( @NotNull UUID uuidTest,Client body,SecurityContext securityContext) throws NotFoundException;
}
