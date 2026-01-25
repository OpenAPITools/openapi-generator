package org.openapitools.api;

import java.util.Date;
import org.openapitools.model.User;

import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.info.Info;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;

/**
 * OpenAPI Petstore
 *
 * <p>This is a sample server Petstore server. For this sample, you can use the api key `special-key` to test the authorization filters.
 *
 */
@Path("/user")
@OpenAPIDefinition(
    info = @Info(
        title = "OpenAPI Petstore",
        description = "This is a sample server Petstore server. For this sample, you can use the api key `special-key` to test the authorization filters.",
        version = "1.0.0"
    )
)
public interface UserApi  {

    /**
     * Create user
     *
     * This can only be done by the logged in user.
     *
     */
    @POST
    
    @Consumes({ "application/json" })
    @Operation(operationId = "createUser", summary = "Create user",  tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    public void createUser(User user);

    /**
     * Creates list of users with given input array
     *
     * 
     *
     */
    @POST
    @Path("/createWithArray")
    @Consumes({ "application/json" })
    @Operation(operationId = "createUsersWithArrayInput", summary = "Creates list of users with given input array",  tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    public void createUsersWithArrayInput(List<User> user);

    /**
     * Creates list of users with given input array
     *
     * 
     *
     */
    @POST
    @Path("/createWithList")
    @Consumes({ "application/json" })
    @Operation(operationId = "createUsersWithListInput", summary = "Creates list of users with given input array",  tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    public void createUsersWithListInput(List<User> user);

    /**
     * Delete user
     *
     * This can only be done by the logged in user.
     *
     */
    @DELETE
    @Path("/{username}")
    @Operation(operationId = "deleteUser", summary = "Delete user",  tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid username supplied"),
        @ApiResponse(responseCode = "404", description = "User not found") })
    public void deleteUser(@PathParam("username") String username);

    /**
     * Get user by user name
     *
     * 
     *
     */
    @GET
    @Path("/{username}")
    @Produces({ "application/xml", "application/json" })
    @Operation(operationId = "getUserByName", summary = "Get user by user name",  tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = User.class))),
        @ApiResponse(responseCode = "400", description = "Invalid username supplied"),
        @ApiResponse(responseCode = "404", description = "User not found") })
    public User getUserByName(@PathParam("username") String username);

    /**
     * Logs user into the system
     *
     * 
     *
     */
    @GET
    @Path("/login")
    @Produces({ "application/xml", "application/json" })
    @Operation(operationId = "loginUser", summary = "Logs user into the system",  tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = String.class))),
        @ApiResponse(responseCode = "400", description = "Invalid username/password supplied") })
    public String loginUser(@QueryParam("username") String username, @QueryParam("password") String password);

    /**
     * Logs out current logged in user session
     *
     * 
     *
     */
    @GET
    @Path("/logout")
    @Operation(operationId = "logoutUser", summary = "Logs out current logged in user session",  tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation") })
    public void logoutUser();

    /**
     * Updated user
     *
     * This can only be done by the logged in user.
     *
     */
    @PUT
    @Path("/{username}")
    @Consumes({ "application/json" })
    @Operation(operationId = "updateUser", summary = "Updated user",  tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid user supplied"),
        @ApiResponse(responseCode = "404", description = "User not found") })
    public void updateUser(@PathParam("username") String username, User user);
}
