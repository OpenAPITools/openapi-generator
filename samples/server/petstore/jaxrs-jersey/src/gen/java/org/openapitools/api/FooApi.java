package org.openapitools.api;

import org.openapitools.model.*;
import org.openapitools.api.FooApiService;
import org.openapitools.api.factories.FooApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import org.openapitools.model.InlineResponseDefault;

import java.util.Map;
import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import org.glassfish.jersey.media.multipart.FormDataParam;
import org.glassfish.jersey.media.multipart.FormDataBodyPart;

import javax.servlet.ServletConfig;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/foo")


@io.swagger.annotations.Api(description = "the foo API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen")
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

    @javax.ws.rs.GET
    
    
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "", response = InlineResponseDefault.class, tags={  })
    @io.swagger.annotations.ApiResponses(value = {
        @io.swagger.annotations.ApiResponse(code = 200, message = "response", response = InlineResponseDefault.class)
    })
    public Response fooGet(@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.fooGet(securityContext);
    }
}
