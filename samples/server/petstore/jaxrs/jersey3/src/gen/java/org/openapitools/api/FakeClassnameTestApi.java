package org.openapitools.api;

import org.openapitools.api.FakeClassnameTestApiService;
import org.openapitools.api.factories.FakeClassnameTestApiServiceFactory;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;

import org.openapitools.model.Client;

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

@Path("/fake_classname_test")


@Tag(description = "the fake_classname_test API", name = "")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen")
public class FakeClassnameTestApi  {
   private final FakeClassnameTestApiService delegate;

   public FakeClassnameTestApi(@Context ServletConfig servletContext) {
      FakeClassnameTestApiService delegate = null;

      if (servletContext != null) {
         String implClass = servletContext.getInitParameter("FakeClassnameTestApi.implementation");
         if (implClass != null && !"".equals(implClass.trim())) {
            try {
               delegate = (FakeClassnameTestApiService) Class.forName(implClass).newInstance();
            } catch (Exception e) {
               throw new RuntimeException(e);
            }
         }
      }

      if (delegate == null) {
         delegate = FakeClassnameTestApiServiceFactory.getFakeClassnameTestApi();
      }

      this.delegate = delegate;
   }

    @jakarta.ws.rs.PATCH
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @Operation(summary = "To test class name in snake case", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Client.class))),
            }
    , tags={ "fake_classname_tags 123#$%^", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "To test class name in snake case", notes = "To test class name in snake case", response = Client.class, authorizations = {
//        @io.swagger.annotations.Authorization(value = "api_key_query")
//    }, tags={ "fake_classname_tags 123#$%^", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Client.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response testClassname(@Schema(description = "client model", required = true) @NotNull @Valid  Client client,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testClassname(client, securityContext);
    }
}
