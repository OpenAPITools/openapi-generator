package org.openapitools.api;

import org.openapitools.api.AnotherFakeApiService;
import org.openapitools.api.factories.AnotherFakeApiServiceFactory;

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

@Path("/another-fake/dummy")


@Tag(description = "the another-fake API", name = "")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen")
public class AnotherFakeApi  {

   private final AnotherFakeApiService delegate;

   public AnotherFakeApi(@Context ServletConfig servletContext) {

      AnotherFakeApiService delegate = null;
      if (servletContext != null) {
         String implClass = servletContext.getInitParameter("AnotherFakeApi.implementation");
         if (implClass != null && !"".equals(implClass.trim())) {
            try {
               delegate = (AnotherFakeApiService) Class.forName(implClass).getDeclaredConstructor().newInstance();
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


    @jakarta.ws.rs.PATCH
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @Operation(summary = "To test special tags", description = "", responses = {
            @ApiResponse(responseCode = "200", description = "successful operation", content = 
                @Content(schema = @Schema(implementation = Client.class))),
            }, tags={ "$another-fake?", }) 
    public Response call123testSpecialTags(@Schema(description = "client model", required = true) @NotNull @Valid  Client client,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.call123testSpecialTags(client, securityContext);
    }
}
