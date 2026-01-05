package org.openapitools.api;

import java.util.Date;
import org.openapitools.model.User;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.v3.oas.annotations.*;
import io.swagger.v3.oas.annotations.media.*;
import io.swagger.v3.oas.annotations.responses.*;
import io.swagger.v3.oas.annotations.tags.Tag;

import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

/**
* Represents a collection of functions to interact with the API endpoints.
*/
@Path("/user")
@Tag(name = "user")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.19.0-SNAPSHOT")
public class UserApi {

    @POST
    @Consumes({ "application/json" })
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
    @Operation(summary = "Creates list of users with given input array", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation")
    })
    public Response createUsersWithListInput(@Valid @NotNull List<@Valid User> user) {
        return Response.ok().entity("magic!").build();
    }

    @DELETE
    @Path("/{username}")
    @Operation(summary = "Delete user", description = "This can only be done by the logged in user.")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid username supplied"),
        @ApiResponse(responseCode = "404", description = "User not found")
    })
    public Response deleteUser(@PathParam("username") String username) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/{username}")
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Get user by user name", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation"),
        @ApiResponse(responseCode = "400", description = "Invalid username supplied"),
        @ApiResponse(responseCode = "404", description = "User not found")
    })
    public Response getUserByName(@PathParam("username") String username) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/login")
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Logs user into the system", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation"),
        @ApiResponse(responseCode = "400", description = "Invalid username/password supplied")
    })
    public Response loginUser(@QueryParam("username") @NotNull   String username,@QueryParam("password") @NotNull   String password) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/logout")
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
    @Operation(summary = "Updated user", description = "This can only be done by the logged in user.")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid user supplied"),
        @ApiResponse(responseCode = "404", description = "User not found")
    })
    public Response updateUser(@PathParam("username") String username,@Valid @NotNull User user) {
        return Response.ok().entity("magic!").build();
    }
}
