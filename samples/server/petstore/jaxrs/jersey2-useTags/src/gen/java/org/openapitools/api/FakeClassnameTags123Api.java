package org.openapitools.api;

import org.openapitools.model.*;
import org.openapitools.api.FakeClassnameTags123ApiService;
import org.openapitools.api.factories.FakeClassnameTags123ApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import org.openapitools.model.Client;

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

@Path("/fake_classname_test")


@io.swagger.annotations.Api(description = "the FakeClassnameTags123 API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen")
public class FakeClassnameTags123Api  {
   private final FakeClassnameTags123ApiService delegate;

   public FakeClassnameTags123Api(@Context ServletConfig servletContext) {
      FakeClassnameTags123ApiService delegate = null;

      if (servletContext != null) {
         String implClass = servletContext.getInitParameter("FakeClassnameTags123Api.implementation");
         if (implClass != null && !"".equals(implClass.trim())) {
            try {
               delegate = (FakeClassnameTags123ApiService) Class.forName(implClass).newInstance();
            } catch (Exception e) {
               throw new RuntimeException(e);
            }
         } 
      }

      if (delegate == null) {
         delegate = FakeClassnameTags123ApiServiceFactory.getFakeClassnameTags123Api();
      }

      this.delegate = delegate;
   }

    @PATCH
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "To test class name in snake case", notes = "To test class name in snake case", response = Client.class, authorizations = {
        @io.swagger.annotations.Authorization(value = "api_key_query")
    }, tags={ "fake_classname_tags 123#$%^", })
    @io.swagger.annotations.ApiResponses(value = {
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Client.class)
    })
    public Response testClassname(@ApiParam(value = "client model", required = true) @NotNull @Valid  Client body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testClassname(body, securityContext);
    }
}
