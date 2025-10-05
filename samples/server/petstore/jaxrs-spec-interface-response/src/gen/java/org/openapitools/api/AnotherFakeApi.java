package org.openapitools.api;

import org.openapitools.model.Client;
import java.util.UUID;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

/**
* Represents a collection of functions to interact with the API endpoints.
*/
@Path("/another-fake/dummy")
@Api(description = "the another-fake API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public interface AnotherFakeApi {

    /**
     * To test special tags and operation ID starting with number
     *
     * @param uuidTest to test uuid example value
     * @param body client model
     * @return successful operation
     */
    @PATCH
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @ApiOperation(value = "To test special tags", notes = "To test special tags and operation ID starting with number", tags={ "$another-fake?" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    Response call123testSpecialTags(@HeaderParam("uuid_test") @NotNull   @ApiParam("to test uuid example value") UUID uuidTest,@Valid @NotNull Client body);

}
