package io.swagger.api;

import java.util.List;
import io.swagger.model.User;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;
import java.lang.Exception;

@Path("/user")

@Api(description = "the user API")




public class UserApi  {

    @POST
    
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Create user", notes = "This can only be done by the logged in user.", response = Void.class, tags={ "user",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    public Response createUser(@Valid User body) throws Exception {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/createWithArray")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Creates list of users with given input array", notes = "", response = Void.class, tags={ "user",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    public Response createUsersWithArrayInput(@Valid List<User> body) throws Exception {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/createWithList")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Creates list of users with given input array", notes = "", response = Void.class, tags={ "user",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    public Response createUsersWithListInput(@Valid List<User> body) throws Exception {
        return Response.ok().entity("magic!").build();
    }

    @DELETE
    @Path("/{username}")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Delete user", notes = "This can only be done by the logged in user.", response = Void.class, tags={ "user",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class) })
    public Response deleteUser(@PathParam("username") @ApiParam("The name that needs to be deleted") String username) throws Exception {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/{username}")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Get user by user name", notes = "", response = User.class, tags={ "user",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = User.class),
        @ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class) })
    public Response getUserByName(@PathParam("username") @ApiParam("The name that needs to be fetched. Use user1 for testing. ") String username) throws Exception {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/login")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Logs user into the system", notes = "", response = String.class, tags={ "user",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = String.class),
        @ApiResponse(code = 400, message = "Invalid username/password supplied", response = Void.class) })
    public Response loginUser(@QueryParam("username") @NotNull   @ApiParam("The user name for login")  String username,@QueryParam("password") @NotNull   @ApiParam("The password for login in clear text")  String password) throws Exception {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/logout")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Logs out current logged in user session", notes = "", response = Void.class, tags={ "user",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    public Response logoutUser() throws Exception {
        return Response.ok().entity("magic!").build();
    }

    @PUT
    @Path("/{username}")
    
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Updated user", notes = "This can only be done by the logged in user.", response = Void.class, tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid user supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class) })
    public Response updateUser(@PathParam("username") @ApiParam("name that need to be deleted") String username,@Valid User body) throws Exception {
        return Response.ok().entity("magic!").build();
    }
}
