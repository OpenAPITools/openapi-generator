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

@Path("/another-fake/dummy")
@Api(description = "the AnotherFake API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")public interface AnotherFakeApi {

    @PATCH
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @ApiOperation(value = "To test special tags", notes = "To test special tags and operation ID starting with number", tags={ "$another-fake?" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    Call123testSpecialTagsResponse call123testSpecialTags(@Valid @NotNull Client body);

    public static class Call123testSpecialTagsResponse extends org.openapitools.api.support.ResponseWrapper {
        private Call123testSpecialTagsResponse(Response delegate) {
            super(delegate);
        }
        public static Call123testSpecialTagsResponse with200ApplicationJson(Client entity) {
            return new Call123testSpecialTagsResponse(Response.status(200).header("Content-Type", "application/json").entity(entity).build());
        }
        public static Call123testSpecialTagsResponse withCustomResponse(Response response) {
            return new Call123testSpecialTagsResponse(response);
        }
    }
}
