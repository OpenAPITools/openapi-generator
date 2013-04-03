package com.wordnik.api;

import com.wordnik.swagger.annotations.*;

import com.wordnik.client.model.User;
import java.util.List;
import com.wordnik.api.NotFoundException;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/user.json")
@Api(value = "/user", description = "the user API")
@Produces({"application/json"})
public class UserApi {
  @POST
  @Path("/createWithArray")
  @ApiOperation(value = "Creates list of users with given input array", notes = "", responseClass = "void")
  @ApiErrors(value = { @ApiError(code = 400, reason = "Invalid ID supplied"),
      @ApiError(code = 404, reason = "Pet not found") })
  public Response createUsersWithArrayInput(
    @ApiParam(value = "List of user object"
    ,required=true) List<User> body
    )
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponse(ApiResponse.OK, "magic!")).build();
  }

  @POST
  @Path("/")
  @ApiOperation(value = "Create user", notes = "This can only be done by the logged in user.", responseClass = "void")
  @ApiErrors(value = { @ApiError(code = 400, reason = "Invalid ID supplied"),
      @ApiError(code = 404, reason = "Pet not found") })
  public Response createUser(
    @ApiParam(value = "Created user object"
    ,required=true) User body
    )
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponse(ApiResponse.OK, "magic!")).build();
  }

  @POST
  @Path("/createWithList")
  @ApiOperation(value = "Creates list of users with given list input", notes = "", responseClass = "void")
  @ApiErrors(value = { @ApiError(code = 400, reason = "Invalid ID supplied"),
      @ApiError(code = 404, reason = "Pet not found") })
  public Response createUsersWithListInput(
    @ApiParam(value = "List of user object"
    ,required=true) List<User> body
    )
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponse(ApiResponse.OK, "magic!")).build();
  }

  @PUT
  @Path("/{username}")
  @ApiOperation(value = "Updated user", notes = "This can only be done by the logged in user.", responseClass = "void")
  @ApiErrors(value = { @ApiError(code = 400, reason = "Invalid ID supplied"),
      @ApiError(code = 404, reason = "Pet not found") })
  public Response updateUser(
    @ApiParam(value = "name that need to be deleted"
    ,required=true)@PathParam("username") String username
    ,@ApiParam(value = "Updated user object"
    ,required=true) User body
    )
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponse(ApiResponse.OK, "magic!")).build();
  }

  @DELETE
  @Path("/{username}")
  @ApiOperation(value = "Delete user", notes = "This can only be done by the logged in user.", responseClass = "void")
  @ApiErrors(value = { @ApiError(code = 400, reason = "Invalid ID supplied"),
      @ApiError(code = 404, reason = "Pet not found") })
  public Response deleteUser(
    @ApiParam(value = "The name that needs to be deleted"
    ,required=true)@PathParam("username") String username
    )
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponse(ApiResponse.OK, "magic!")).build();
  }

  @GET
  @Path("/{username}")
  @ApiOperation(value = "Get user by user name", notes = "", responseClass = "User")
  @ApiErrors(value = { @ApiError(code = 400, reason = "Invalid ID supplied"),
      @ApiError(code = 404, reason = "Pet not found") })
  public Response getUserByName(
    @ApiParam(value = "The name that needs to be fetched. Use user1 for testing."
    ,required=true)@PathParam("username") String username
    )
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponse(ApiResponse.OK, "magic!")).build();
  }

  @GET
  @Path("/login")
  @ApiOperation(value = "Logs user into the system", notes = "", responseClass = "String")
  @ApiErrors(value = { @ApiError(code = 400, reason = "Invalid ID supplied"),
      @ApiError(code = 404, reason = "Pet not found") })
  public Response loginUser(
    @ApiParam(value = "The user name for login"
    ,required=true)@QueryParam("username") String username
    ,@ApiParam(value = "The password for login in clear text"
    ,required=true)@QueryParam("password") String password
    )
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponse(ApiResponse.OK, "magic!")).build();
  }

  @GET
  @Path("/logout")
  @ApiOperation(value = "Logs out current logged in user session", notes = "", responseClass = "void")
  @ApiErrors(value = { @ApiError(code = 400, reason = "Invalid ID supplied"),
      @ApiError(code = 404, reason = "Pet not found") })
  public Response logoutUser(
    )
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponse(ApiResponse.OK, "magic!")).build();
  }

  }

