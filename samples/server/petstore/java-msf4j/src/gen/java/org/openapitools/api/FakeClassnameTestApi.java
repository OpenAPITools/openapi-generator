package org.openapitools.api;

import org.openapitools.model.*;
import org.openapitools.api.FakeClassnameTestApiService;
import org.openapitools.api.factories.FakeClassnameTestApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import org.openapitools.model.Client;

import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import org.wso2.msf4j.formparam.FormDataParam;
import org.wso2.msf4j.formparam.FileInfo;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;

@Path("/fake_classname_test")


@io.swagger.annotations.Api(description = "the fake_classname_test API")

public class FakeClassnameTestApi  {
   private final FakeClassnameTestApiService delegate = FakeClassnameTestApiServiceFactory.getFakeClassnameTestApi();

    @PATCH
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "To test class name in snake case", notes = "To test class name in snake case", response = Client.class, authorizations = {
        @io.swagger.annotations.Authorization(value = "api_key_query")
    }, tags={ "fake_classname_tags 123#$%^", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    public Response testClassname(@ApiParam(value = "client model" ,required=true) Client body
)
    throws NotFoundException {
        return delegate.testClassname(body);
    }
}
