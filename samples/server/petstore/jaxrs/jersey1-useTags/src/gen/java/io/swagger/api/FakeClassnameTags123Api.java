package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.FakeClassnameTags123ApiService;
import io.swagger.api.factories.FakeClassnameTags123ApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import com.sun.jersey.multipart.FormDataParam;
import javax.validation.constraints.*;

import io.swagger.model.Client;

import java.util.Map;
import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import com.sun.jersey.core.header.FormDataContentDisposition;
import com.sun.jersey.multipart.FormDataParam;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;

@Path("/FakeClassnameTags123")


@io.swagger.annotations.Api(description = "the FakeClassnameTags123 API")

public class FakeClassnameTags123Api  {
   private final FakeClassnameTags123ApiService delegate = FakeClassnameTags123ApiServiceFactory.getFakeClassnameTags123Api();

    @PATCH
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "To test class name in snake case", notes = "", response = Client.class, authorizations = {
        @io.swagger.annotations.Authorization(value = "api_key_query")
    }, tags={ "fake_classname_tags 123#$%^" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    public Response testClassname(
        @ApiParam(value = "client model" ,required=true) Client body,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testClassname(body,securityContext);
    }
}
