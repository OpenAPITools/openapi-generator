package org.openapitools.api;

import org.openapitools.api.*;
import org.openapitools.model.*;

import javax.validation.constraints.*;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSCXFCDIServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public interface TestApiService {
      public Response testUpload(java.io.InputStream body, SecurityContext securityContext);
}
