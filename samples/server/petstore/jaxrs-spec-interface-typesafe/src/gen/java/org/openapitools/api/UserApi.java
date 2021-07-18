package org.openapitools.api;

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
@Api(description = "the User API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")public interface UserApi {

    @POST
    @ApiOperation(value = "Create user", notes = "This can only be done by the logged in user.", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    CreateUserResponse createUser(@Valid @NotNull User body);

    public static class CreateUserResponse extends org.openapitools.api.support.ResponseWrapper {
        private CreateUserResponse(Response delegate) {
            super(delegate);
        }
        public static CreateUserResponse with200() {
            return new CreateUserResponse(Response.status(200).build());
        }
        public static CreateUserResponse withCustomResponse(Response response) {
            return new CreateUserResponse(response);
        }
    }

    @POST
    @Path("/createWithArray")
    @ApiOperation(value = "Creates list of users with given input array", notes = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    CreateUsersWithArrayInputResponse createUsersWithArrayInput(@Valid @NotNull List<User> body);

    public static class CreateUsersWithArrayInputResponse extends org.openapitools.api.support.ResponseWrapper {
        private CreateUsersWithArrayInputResponse(Response delegate) {
            super(delegate);
        }
        public static CreateUsersWithArrayInputResponse with200() {
            return new CreateUsersWithArrayInputResponse(Response.status(200).build());
        }
        public static CreateUsersWithArrayInputResponse withCustomResponse(Response response) {
            return new CreateUsersWithArrayInputResponse(response);
        }
    }

    @POST
    @Path("/createWithList")
    @ApiOperation(value = "Creates list of users with given input array", notes = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    CreateUsersWithListInputResponse createUsersWithListInput(@Valid @NotNull List<User> body);

    public static class CreateUsersWithListInputResponse extends org.openapitools.api.support.ResponseWrapper {
        private CreateUsersWithListInputResponse(Response delegate) {
            super(delegate);
        }
        public static CreateUsersWithListInputResponse with200() {
            return new CreateUsersWithListInputResponse(Response.status(200).build());
        }
        public static CreateUsersWithListInputResponse withCustomResponse(Response response) {
            return new CreateUsersWithListInputResponse(response);
        }
    }

    @DELETE
    @Path("/{username}")
    @ApiOperation(value = "Delete user", notes = "This can only be done by the logged in user.", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class) })
    DeleteUserResponse deleteUser(@PathParam("username") @ApiParam("The name that needs to be deleted") String username);

    public static class DeleteUserResponse extends org.openapitools.api.support.ResponseWrapper {
        private DeleteUserResponse(Response delegate) {
            super(delegate);
        }
        public static DeleteUserResponse with400() {
            return new DeleteUserResponse(Response.status(400).build());
        }
        public static DeleteUserResponse with404() {
            return new DeleteUserResponse(Response.status(404).build());
        }
        public static DeleteUserResponse withCustomResponse(Response response) {
            return new DeleteUserResponse(response);
        }
    }

    @GET
    @Path("/{username}")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Get user by user name", notes = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = User.class),
        @ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class) })
    GetUserByNameResponse getUserByName(@PathParam("username") @ApiParam("The name that needs to be fetched. Use user1 for testing.") String username);

    public static class GetUserByNameResponse extends org.openapitools.api.support.ResponseWrapper {
        private GetUserByNameResponse(Response delegate) {
            super(delegate);
        }
        public static GetUserByNameResponse with200ApplicationXml(User entity) {
            return new GetUserByNameResponse(Response.status(200).header("Content-Type", "application/xml").entity(entity).build());
        }
        public static GetUserByNameResponse with200ApplicationJson(User entity) {
            return new GetUserByNameResponse(Response.status(200).header("Content-Type", "application/json").entity(entity).build());
        }
        public static GetUserByNameResponse with400() {
            return new GetUserByNameResponse(Response.status(400).build());
        }
        public static GetUserByNameResponse with404() {
            return new GetUserByNameResponse(Response.status(404).build());
        }
        public static GetUserByNameResponse withCustomResponse(Response response) {
            return new GetUserByNameResponse(response);
        }
    }

    @GET
    @Path("/login")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Logs user into the system", notes = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = String.class),
        @ApiResponse(code = 400, message = "Invalid username/password supplied", response = Void.class) })
    LoginUserResponse loginUser(@QueryParam("username") @NotNull   @ApiParam("The user name for login")  String username,@QueryParam("password") @NotNull   @ApiParam("The password for login in clear text")  String password);

    public static class LoginUserResponse extends org.openapitools.api.support.ResponseWrapper {
        private LoginUserResponse(Response delegate) {
            super(delegate);
        }
        public static LoginUserResponse with200ApplicationXml(String entity) {
            return new LoginUserResponse(Response.status(200).header("Content-Type", "application/xml").entity(entity).build());
        }
        public static LoginUserResponse with200ApplicationJson(String entity) {
            return new LoginUserResponse(Response.status(200).header("Content-Type", "application/json").entity(entity).build());
        }
        public static LoginUserResponse with400() {
            return new LoginUserResponse(Response.status(400).build());
        }
        public static LoginUserResponse withCustomResponse(Response response) {
            return new LoginUserResponse(response);
        }
    }

    @GET
    @Path("/logout")
    @ApiOperation(value = "Logs out current logged in user session", notes = "", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    LogoutUserResponse logoutUser();

    public static class LogoutUserResponse extends org.openapitools.api.support.ResponseWrapper {
        private LogoutUserResponse(Response delegate) {
            super(delegate);
        }
        public static LogoutUserResponse with200() {
            return new LogoutUserResponse(Response.status(200).build());
        }
        public static LogoutUserResponse withCustomResponse(Response response) {
            return new LogoutUserResponse(response);
        }
    }

    @PUT
    @Path("/{username}")
    @ApiOperation(value = "Updated user", notes = "This can only be done by the logged in user.", tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid user supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class) })
    UpdateUserResponse updateUser(@PathParam("username") @ApiParam("name that need to be deleted") String username,@Valid @NotNull User body);

    public static class UpdateUserResponse extends org.openapitools.api.support.ResponseWrapper {
        private UpdateUserResponse(Response delegate) {
            super(delegate);
        }
        public static UpdateUserResponse with400() {
            return new UpdateUserResponse(Response.status(400).build());
        }
        public static UpdateUserResponse with404() {
            return new UpdateUserResponse(Response.status(404).build());
        }
        public static UpdateUserResponse withCustomResponse(Response response) {
            return new UpdateUserResponse(response);
        }
    }
}
