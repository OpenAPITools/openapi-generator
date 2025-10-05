package org.openapitools.api;

import java.util.Date;
import org.openapitools.model.User;

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
@Path("/user")
@Api(description = "the user API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public interface UserApi {

    /**
     * This can only be done by the logged in user.
     *
     * @param body Created user object
     * @return successful operation
     */
    @POST
    @ApiOperation(value = "Create user", notes = "This can only be done by the logged in user.", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    Response createUser(@Valid @NotNull User body);


    /**
     * 
     *
     * @param body List of user object
     * @return successful operation
     */
    @POST
    @Path("/createWithArray")
    @ApiOperation(value = "Creates list of users with given input array", notes = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    Response createUsersWithArrayInput(@Valid @NotNull List<@Valid User> body);


    /**
     * 
     *
     * @param body List of user object
     * @return successful operation
     */
    @POST
    @Path("/createWithList")
    @ApiOperation(value = "Creates list of users with given input array", notes = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    Response createUsersWithListInput(@Valid @NotNull List<@Valid User> body);


    /**
     * This can only be done by the logged in user.
     *
     * @param username The name that needs to be deleted
     * @return Invalid username supplied
     * @return User not found
     */
    @DELETE
    @Path("/{username}")
    @ApiOperation(value = "Delete user", notes = "This can only be done by the logged in user.", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class) })
    Response deleteUser(@PathParam("username") @ApiParam("The name that needs to be deleted") String username);


    /**
     * 
     *
     * @param username The name that needs to be fetched. Use user1 for testing.
     * @return successful operation
     * @return Invalid username supplied
     * @return User not found
     */
    @GET
    @Path("/{username}")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Get user by user name", notes = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = User.class),
        @ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class) })
    Response getUserByName(@PathParam("username") @ApiParam("The name that needs to be fetched. Use user1 for testing.") String username);


    /**
     * 
     *
     * @param username The user name for login
     * @param password The password for login in clear text
     * @return successful operation
     * @return Invalid username/password supplied
     */
    @GET
    @Path("/login")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Logs user into the system", notes = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = String.class),
        @ApiResponse(code = 400, message = "Invalid username/password supplied", response = Void.class) })
    Response loginUser(@QueryParam("username") @NotNull  @ApiParam("The user name for login")  String username,@QueryParam("password") @NotNull  @ApiParam("The password for login in clear text")  String password);


    /**
     * 
     *
     * @return successful operation
     */
    @GET
    @Path("/logout")
    @ApiOperation(value = "Logs out current logged in user session", notes = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    Response logoutUser();


    /**
     * This can only be done by the logged in user.
     *
     * @param username name that need to be deleted
     * @param body Updated user object
     * @return Invalid user supplied
     * @return User not found
     */
    @PUT
    @Path("/{username}")
    @ApiOperation(value = "Updated user", notes = "This can only be done by the logged in user.", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid user supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class) })
    Response updateUser(@PathParam("username") @ApiParam("name that need to be deleted") String username,@Valid @NotNull User body);

}
