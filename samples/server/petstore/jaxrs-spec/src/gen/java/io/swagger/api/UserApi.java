package io.swagger.api;

import java.util.List;
import io.swagger.model.User;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.util.List;
import javax.validation.constraints.*;

@Path("/user")

@Api(description = "the user API")




public class UserApi  {

    @POST
    
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Create user", notes = "This can only be done by the logged in user.", response = void.class, tags={ "user",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = void.class) })
    public Response createUser(User body) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/createWithArray")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Creates list of users with given input array", notes = "", response = void.class, tags={ "user",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = void.class) })
    public Response createUsersWithArrayInput(List<User> body) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/createWithList")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Creates list of users with given input array", notes = "", response = void.class, tags={ "user",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = void.class) })
    public Response createUsersWithListInput(List<User> body) {
        return Response.ok().entity("magic!").build();
    }

    @DELETE
    @Path("/{username}")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Delete user", notes = "This can only be done by the logged in user.", response = void.class, tags={ "user",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied", response = void.class),
        @ApiResponse(code = 404, message = "User not found", response = void.class) })
    public Response deleteUser(@PathParam("username") @ApiParam("The name that needs to be deleted") String username) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/{username}")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Get user by user name", notes = "", response = User.class, tags={ "user",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = User.class),
        @ApiResponse(code = 400, message = "Invalid username supplied", response = User.class),
        @ApiResponse(code = 404, message = "User not found", response = User.class) })
    public Response getUserByName(@PathParam("username") @ApiParam("The name that needs to be fetched. Use user1 for testing. ") String username) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/login")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Logs user into the system", notes = "", response = String.class, tags={ "user",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = String.class),
        @ApiResponse(code = 400, message = "Invalid username/password supplied", response = String.class) })
    public Response loginUser(@QueryParam("username") @NotNull  String username,@QueryParam("password") @NotNull  String password) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/logout")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Logs out current logged in user session", notes = "", response = void.class, tags={ "user",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = void.class) })
    public Response logoutUser() {
        return Response.ok().entity("magic!").build();
    }

    @PUT
    @Path("/{username}")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Updated user", notes = "This can only be done by the logged in user.", response = void.class, tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid user supplied", response = void.class),
        @ApiResponse(code = 404, message = "User not found", response = void.class) })
    public Response updateUser(@PathParam("username") @ApiParam("name that need to be deleted") String username,User body) {
        return Response.ok().entity("magic!").build();
    }
}

