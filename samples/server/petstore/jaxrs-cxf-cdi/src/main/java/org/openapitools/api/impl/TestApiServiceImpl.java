package org.openapitools.api.impl;

import org.openapitools.api.*;
import org.openapitools.model.*;

import javax.validation.constraints.*;

import javax.enterprise.context.RequestScoped;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@RequestScoped
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSCXFCDIServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public class TestApiServiceImpl implements TestApiService {
      @Override
      public Response testUpload(java.io.InputStream body, SecurityContext securityContext) {
      // do some magic!
      return Response.ok().entity("magic!").build();
  }
}
