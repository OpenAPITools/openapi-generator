package org.openapitools.api;

import org.openapitools.model.Client;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/fake_classname_test")
@Api(description = "the FakeClassnameTags123 API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")public interface FakeClassnameTags123Api {

    @PATCH
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @ApiOperation(value = "To test class name in snake case", notes = "To test class name in snake case", authorizations = {
        
        @Authorization(value = "api_key_query")
         }, tags={ "fake_classname_tags 123#$%^" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    TestClassnameResponse testClassname(@Valid @NotNull Client body);

    public static class TestClassnameResponse extends org.openapitools.api.support.ResponseWrapper {
        private TestClassnameResponse(Response delegate) {
            super(delegate);
        }
        public static TestClassnameResponse with200ApplicationJson(Client entity) {
            return new TestClassnameResponse(Response.status(200).header("Content-Type", "application/json").entity(entity).build());
        }
        public static TestClassnameResponse withCustomResponse(Response response) {
            return new TestClassnameResponse(response);
        }
    }
}
