package io.swagger.api;

import io.swagger.model.*;

import com.wordnik.swagger.annotations.ApiParam;

import com.sun.jersey.multipart.FormDataParam;

import io.swagger.model.User;
import java.util.*;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import com.sun.jersey.core.header.FormDataContentDisposition;
import com.sun.jersey.multipart.FormDataParam;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/user")
@com.wordnik.swagger.annotations.Api(value = "/user", description = "the user API")
public class UserApi {
  
  @POST
  
  
  @Produces({ "application/json", "application/xml" })
  @com.wordnik.swagger.annotations.ApiOperation(value = "Create user", notes = "This can only be done by the logged in user.", response = Void.class)
  @com.wordnik.swagger.annotations.ApiResponses(value = {  })

  public Response createUser(@ApiParam(value = "Created user object"  ) User body)
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
  }

  
  @POST
  @Path("/createWithArray")
  
  @Produces({ "application/json", "application/xml" })
  @com.wordnik.swagger.annotations.ApiOperation(value = "Creates list of users with given input array", notes = "", response = Void.class)
  @com.wordnik.swagger.annotations.ApiResponses(value = {  })

  public Response createUsersWithArrayInput(@ApiParam(value = "List of user object"  ) List<User> body)
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
  }

  
  @POST
  @Path("/createWithList")
  
  @Produces({ "application/json", "application/xml" })
  @com.wordnik.swagger.annotations.ApiOperation(value = "Creates list of users with given input array", notes = "", response = Void.class)
  @com.wordnik.swagger.annotations.ApiResponses(value = {  })

  public Response createUsersWithListInput(@ApiParam(value = "List of user object"  ) List<User> body)
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
  }

  
  @GET
  @Path("/login")
  
  @Produces({ "application/json", "application/xml" })
  @com.wordnik.swagger.annotations.ApiOperation(value = "Logs user into the system", notes = "", response = String.class)
  @com.wordnik.swagger.annotations.ApiResponses(value = { 
    @com.wordnik.swagger.annotations.ApiResponse(code = 400, message = "Invalid username/password supplied") })

  public Response loginUser(@ApiParam(value = "The user name for login") @QueryParam("username") String username,
    @ApiParam(value = "The password for login in clear text") @QueryParam("password") String password)
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
  }

  
  @GET
  @Path("/logout")
  
  @Produces({ "application/json", "application/xml" })
  @com.wordnik.swagger.annotations.ApiOperation(value = "Logs out current logged in user session", notes = "", response = Void.class)
  @com.wordnik.swagger.annotations.ApiResponses(value = {  })

  public Response logoutUser()
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
  }

  
  @GET
  @Path("/{username}")
  
  @Produces({ "application/json", "application/xml" })
  @com.wordnik.swagger.annotations.ApiOperation(value = "Get user by user name", notes = "", response = User.class)
  @com.wordnik.swagger.annotations.ApiResponses(value = { 
    @com.wordnik.swagger.annotations.ApiResponse(code = 404, message = "User not found"),
    
    @com.wordnik.swagger.annotations.ApiResponse(code = 400, message = "Invalid username supplied") })

  public Response getUserByName(@ApiParam(value = "The name that needs to be fetched. Use user1 for testing. ",required=true ) @PathParam("username") String username)
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
  }

  
  @PUT
  @Path("/{username}")
  
  @Produces({ "application/json", "application/xml" })
  @com.wordnik.swagger.annotations.ApiOperation(value = "Updated user", notes = "This can only be done by the logged in user.", response = Void.class)
  @com.wordnik.swagger.annotations.ApiResponses(value = { 
    @com.wordnik.swagger.annotations.ApiResponse(code = 404, message = "User not found"),
    
    @com.wordnik.swagger.annotations.ApiResponse(code = 400, message = "Invalid user supplied") })

  public Response updateUser(@ApiParam(value = "name that need to be deleted",required=true ) @PathParam("username") String username,
    @ApiParam(value = "Updated user object"  ) User body)
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
  }

  
  @DELETE
  @Path("/{username}")
  
  @Produces({ "application/json", "application/xml" })
  @com.wordnik.swagger.annotations.ApiOperation(value = "Delete user", notes = "This can only be done by the logged in user.", response = Void.class)
  @com.wordnik.swagger.annotations.ApiResponses(value = { 
    @com.wordnik.swagger.annotations.ApiResponse(code = 404, message = "User not found"),
    
    @com.wordnik.swagger.annotations.ApiResponse(code = 400, message = "Invalid username supplied") })

  public Response deleteUser(@ApiParam(value = "The name that needs to be deleted",required=true ) @PathParam("username") String username)
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
  }

  
}
