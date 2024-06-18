package org.openapitools.api;

import org.openapitools.api.*;

import org.glassfish.jersey.media.multipart.FormDataBodyPart;

import org.openapitools.model.FooGetDefaultResponse;

import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.SecurityContext;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen", comments = "Generator version: 7.7.0-SNAPSHOT")
public abstract class FooApiService {
    public abstract Response fooGet(SecurityContext securityContext) throws NotFoundException;
}
