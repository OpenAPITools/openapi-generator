package org.openapitools.api;

import org.openapitools.api.FooApiService;
import org.openapitools.api.factories.FooApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import org.openapitools.model.FooGetDefaultResponse;

import java.util.Map;
import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import org.glassfish.jersey.media.multipart.FormDataParam;
import org.glassfish.jersey.media.multipart.FormDataBodyPart;

import jakarta.servlet.ServletConfig;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.SecurityContext;
import jakarta.ws.rs.*;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

@Path("/foo")


@io.swagger.annotations.Api(description = "the foo API")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen", comments = "Generator version: 7.5.0-SNAPSHOT")
public class FooApi  {
   private final FooApiService delegate;

   public FooApi(@Context ServletConfig servletContext) {
      FooApiService delegate = null;

      if (servletContext != null) {
         String implClass = servletContext.getInitParameter("FooApi.implementation");
         if (implClass != null && !"".equals(implClass.trim())) {
            try {
               delegate = (FooApiService) Class.forName(implClass).newInstance();
            } catch (Exception e) {
               throw new RuntimeException(e);
            }
         }
      }

      if (delegate == null) {
         delegate = FooApiServiceFactory.getFooApi();
      }

      this.delegate = delegate;
   }

    @jakarta.ws.rs.GET
    
    
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "", response = FooGetDefaultResponse.class, tags={  })
    @io.swagger.annotations.ApiResponses(value = {
        @io.swagger.annotations.ApiResponse(code = 200, message = "response", response = FooGetDefaultResponse.class)
    })
    public Response fooGet(@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.fooGet(securityContext);
    }
}
