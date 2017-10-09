package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.AnotherFakeApiService;
import io.swagger.api.factories.AnotherFakeApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import io.swagger.model.Client;

import java.util.Map;
import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import org.glassfish.jersey.media.multipart.FormDataContentDisposition;
import org.glassfish.jersey.media.multipart.FormDataParam;

import javax.servlet.ServletConfig;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;
import javax.validation.constraints.*;

@Path("/another-fake")


@io.swagger.annotations.Api(description = "the another-fake API")

public class AnotherFakeApi  {
   private final AnotherFakeApiService delegate;

   public AnotherFakeApi(@Context ServletConfig servletContext) {
      AnotherFakeApiService delegate = null;

      if (servletContext != null) {
         String implClass = servletContext.getInitParameter("AnotherFakeApi.implementation");
         if (implClass != null && !"".equals(implClass.trim())) {
            try {
               delegate = (AnotherFakeApiService) Class.forName(implClass).newInstance();
            } catch (Exception e) {
               throw new RuntimeException(e);
            }
         } 
      }

      if (delegate == null) {
         delegate = AnotherFakeApiServiceFactory.getAnotherFakeApi();
      }

      this.delegate = delegate;
   }

    @PATCH
    @Path("/dummy")
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "To test special tags", notes = "To test special tags", response = Client.class, tags={ "$another-fake?", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    public Response testSpecialTags(@ApiParam(value = "client model" ,required=true) Client body
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testSpecialTags(body,securityContext);
    }
}
