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
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.23.0-SNAPSHOT")
public interface UserApi {

    /**
     * This can only be done by the logged in user.
     *
     * @param user Created user object
     * @return successful operation
     */
    @POST
    @Consumes({ "application/json" })
    @ApiOperation(value = "Create user", notes = "This can only be done by the logged in user.", authorizations = {
        
        @Authorization(value = "api_key")
         }, tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    void createUser(@Valid @NotNull User user);


    /**
     * 
     *
     * @param user List of user object
     * @return successful operation
     */
    @POST
    @Path("/createWithArray")
    @Consumes({ "application/json" })
    @ApiOperation(value = "Creates list of users with given input array", notes = "", authorizations = {
        
        @Authorization(value = "api_key")
         }, tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    void createUsersWithArrayInput(@Valid @NotNull List<@Valid User> user);


    /**
     * 
     *
     * @param user List of user object
     * @return successful operation
     */
    @POST
    @Path("/createWithList")
    @Consumes({ "application/json" })
    @ApiOperation(value = "Creates list of users with given input array", notes = "", authorizations = {
        
        @Authorization(value = "api_key")
         }, tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    void createUsersWithListInput(@Valid @NotNull List<@Valid User> user);


    /**
     * This can only be done by the logged in user.
     *
     * @param username The name that needs to be deleted
     * @return Invalid username supplied
     * @return User not found
     */
    @DELETE
    @Path("/{username}")
    @ApiOperation(value = "Delete user", notes = "This can only be done by the logged in user.", authorizations = {
        
        @Authorization(value = "api_key")
         }, tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class) })
    void deleteUser(@PathParam("username") @ApiParam("The name that needs to be deleted") String username);


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
    User getUserByName(@PathParam("username") @ApiParam("The name that needs to be fetched. Use user1 for testing.") String username);


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
    String loginUser(@QueryParam("username") @NotNull @Pattern(regexp="^[a-zA-Z0-9]+[a-zA-Z0-9\\.\\-_]*[a-zA-Z0-9]+$")  @ApiParam("The user name for login")  String username,@QueryParam("password") @NotNull  @ApiParam("The password for login in clear text")  String password);


    /**
     * 
     *
     * @return successful operation
     */
    @GET
    @Path("/logout")
    @ApiOperation(value = "Logs out current logged in user session", notes = "", authorizations = {
        
        @Authorization(value = "api_key")
         }, tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    void logoutUser();


    /**
     * This can only be done by the logged in user.
     *
     * @param username name that need to be deleted
     * @param user Updated user object
     * @return Invalid user supplied
     * @return User not found
     */
    @PUT
    @Path("/{username}")
    @Consumes({ "application/json" })
    @ApiOperation(value = "Updated user", notes = "This can only be done by the logged in user.", authorizations = {
        
        @Authorization(value = "api_key")
         }, tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid user supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class) })
    void updateUser(@PathParam("username") @ApiParam("name that need to be deleted") String username,@Valid @NotNull User user);

}
