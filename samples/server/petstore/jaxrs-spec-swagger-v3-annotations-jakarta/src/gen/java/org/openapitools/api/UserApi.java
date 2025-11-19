package org.openapitools.api;

import java.util.Date;
import org.openapitools.model.User;

import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Response;

import io.swagger.annotations.*;
import io.swagger.v3.oas.annotations.*;
import io.swagger.v3.oas.annotations.media.*;
import io.swagger.v3.oas.annotations.responses.*;
import io.swagger.v3.oas.annotations.tags.Tag;

import java.io.InputStream;
import java.util.Map;
import java.util.List;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

/**
* Represents a collection of functions to interact with the API endpoints.
*/
@Path("/user")
@Api(description = "the user API")
@Tag(name = "user")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.18.0-SNAPSHOT")
public class UserApi {

    @POST
    @Consumes({ "application/json" })
    @ApiOperation(value = "Create user", notes = "This can only be done by the logged in user.", response = Void.class, tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class)
    })
    @Operation(summary = "Create user", description = "This can only be done by the logged in user.")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation")
    })
    public Response createUser(@Valid @NotNull User user) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/createWithArray")
    @Consumes({ "application/json" })
    @ApiOperation(value = "Creates list of users with given input array", notes = "", response = Void.class, tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class)
    })
    @Operation(summary = "Creates list of users with given input array", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation")
    })
    public Response createUsersWithArrayInput(@Valid @NotNull List<@Valid User> user) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/createWithList")
    @Consumes({ "application/json" })
    @ApiOperation(value = "Creates list of users with given input array", notes = "", response = Void.class, tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class)
    })
    @Operation(summary = "Creates list of users with given input array", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation")
    })
    public Response createUsersWithListInput(@Valid @NotNull List<@Valid User> user) {
        return Response.ok().entity("magic!").build();
    }

    @DELETE
    @Path("/{username}")
    @ApiOperation(value = "Delete user", notes = "This can only be done by the logged in user.", response = Void.class, tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class)
    })
    @Operation(summary = "Delete user", description = "This can only be done by the logged in user.")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid username supplied"),
        @ApiResponse(responseCode = "404", description = "User not found")
    })
    public Response deleteUser(@PathParam("username") @ApiParam("The name that needs to be deleted") String username) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/{username}")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Get user by user name", notes = "", response = User.class, tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = User.class),
        @ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class)
    })
    @Operation(summary = "Get user by user name", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation"),
        @ApiResponse(responseCode = "400", description = "Invalid username supplied"),
        @ApiResponse(responseCode = "404", description = "User not found")
    })
    public Response getUserByName(@PathParam("username") @ApiParam("The name that needs to be fetched. Use user1 for testing.") String username) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/login")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Logs user into the system", notes = "", response = String.class, tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = String.class),
        @ApiResponse(code = 400, message = "Invalid username/password supplied", response = Void.class)
    })
    @Operation(summary = "Logs user into the system", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation"),
        @ApiResponse(responseCode = "400", description = "Invalid username/password supplied")
    })
    public Response loginUser(@QueryParam("username") @NotNull  @ApiParam("The user name for login")  String username,@QueryParam("password") @NotNull  @ApiParam("The password for login in clear text")  String password) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/logout")
    @ApiOperation(value = "Logs out current logged in user session", notes = "", response = Void.class, tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class)
    })
    @Operation(summary = "Logs out current logged in user session", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation")
    })
    public Response logoutUser() {
        return Response.ok().entity("magic!").build();
    }

    @PUT
    @Path("/{username}")
    @Consumes({ "application/json" })
    @ApiOperation(value = "Updated user", notes = "This can only be done by the logged in user.", response = Void.class, tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid user supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class)
    })
    @Operation(summary = "Updated user", description = "This can only be done by the logged in user.")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid user supplied"),
        @ApiResponse(responseCode = "404", description = "User not found")
    })
    public Response updateUser(@PathParam("username") @ApiParam("name that need to be deleted") String username,@Valid @NotNull User user) {
        return Response.ok().entity("magic!").build();
    }
}
