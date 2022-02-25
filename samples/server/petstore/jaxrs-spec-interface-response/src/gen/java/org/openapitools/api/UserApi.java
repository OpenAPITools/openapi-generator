package org.openapitools.api;

import java.util.Date;
import java.util.List;
import org.openapitools.model.User;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/user")
@Api(description = "the user API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")public interface UserApi {

    @POST
    @ApiOperation(value = "Create user", notes = "This can only be done by the logged in user.", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    Response createUser(@Valid @NotNull User body);

    @POST
    @Path("/createWithArray")
    @ApiOperation(value = "Creates list of users with given input array", notes = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    Response createUsersWithArrayInput(@Valid @NotNull List<User> body);

    @POST
    @Path("/createWithList")
    @ApiOperation(value = "Creates list of users with given input array", notes = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    Response createUsersWithListInput(@Valid @NotNull List<User> body);

    @DELETE
    @Path("/{username}")
    @ApiOperation(value = "Delete user", notes = "This can only be done by the logged in user.", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class) })
    Response deleteUser(@PathParam("username") @ApiParam("The name that needs to be deleted") String username);

    @GET
    @Path("/{username}")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Get user by user name", notes = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = User.class),
        @ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class) })
    Response getUserByName(@PathParam("username") @ApiParam("The name that needs to be fetched. Use user1 for testing.") String username);

    @GET
    @Path("/login")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Logs user into the system", notes = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = String.class),
        @ApiResponse(code = 400, message = "Invalid username/password supplied", response = Void.class) })
    Response loginUser(@QueryParam("username") @NotNull  @ApiParam("The user name for login")  String username,@QueryParam("password") @NotNull  @ApiParam("The password for login in clear text")  String password);

    @GET
    @Path("/logout")
    @ApiOperation(value = "Logs out current logged in user session", notes = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    Response logoutUser();

    @PUT
    @Path("/{username}")
    @ApiOperation(value = "Updated user", notes = "This can only be done by the logged in user.", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid user supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class) })
    Response updateUser(@PathParam("username") @ApiParam("name that need to be deleted") String username,@Valid @NotNull User body);
}
