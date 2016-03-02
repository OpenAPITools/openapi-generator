package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.PettestingByteArraytrueApiService;
import io.swagger.api.factories.PettestingByteArraytrueApiServiceFactory;



import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;

@Path("/pet?testing_byte_array=true")


@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaResteasyServerCodegen", date = "2016-02-04T01:58:20.368+07:00")

public class PettestingByteArraytrueApi  {
   private final PettestingByteArraytrueApiService delegate = PettestingByteArraytrueApiServiceFactory.getPettestingByteArraytrueApi();


    @POST
    
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/json", "application/xml" })
    public Response addPetUsingByteArray( byte[] body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.addPetUsingByteArray(body,securityContext);
    }

}

