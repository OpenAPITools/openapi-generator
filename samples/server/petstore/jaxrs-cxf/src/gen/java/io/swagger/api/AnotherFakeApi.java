package io.swagger.api;

import io.swagger.model.Client;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.MediaType;
import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponses;
import io.swagger.annotations.ApiResponse;
import io.swagger.jaxrs.PATCH;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/")
@Api(value = "/", description = "")
public interface AnotherFakeApi  {

    @PATCH
    @Path("/another-fake/dummy")
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @ApiOperation(value = "To test special tags", tags={ "$another-fake?" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    public Client testSpecialTags(@Valid Client body);
}

