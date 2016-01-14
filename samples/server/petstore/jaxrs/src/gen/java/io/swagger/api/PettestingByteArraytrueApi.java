package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.PettestingByteArraytrueApiService;
import io.swagger.api.factories.PettestingByteArraytrueApiServiceFactory;

import io.swagger.annotations.ApiParam;

import com.sun.jersey.multipart.FormDataParam;


import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import com.sun.jersey.core.header.FormDataContentDisposition;
import com.sun.jersey.multipart.FormDataParam;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;

@Path("/pet?testing_byte_array=true")


@io.swagger.annotations.Api(description = "the pet?testing_byte_array=true API")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JaxRSServerCodegen", date = "2016-01-14T21:37:36.074Z")
public class PettestingByteArraytrueApi  {
   private final PettestingByteArraytrueApiService delegate = PettestingByteArraytrueApiServiceFactory.getPettestingByteArraytrueApi();

    @POST
    
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/json", "application/xml" })
    @io.swagger.annotations.ApiOperation(value = "Fake endpoint to test byte array in body parameter for adding a new pet to the store", notes = "", response = Void.class, authorizations = {
        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
            @io.swagger.annotations.AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @io.swagger.annotations.AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 405, message = "Invalid input", response = Void.class) })

    public Response addPetUsingByteArray(@ApiParam(value = "Pet object in the form of byte array" ) byte[] body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.addPetUsingByteArray(body,securityContext);
    }
}
