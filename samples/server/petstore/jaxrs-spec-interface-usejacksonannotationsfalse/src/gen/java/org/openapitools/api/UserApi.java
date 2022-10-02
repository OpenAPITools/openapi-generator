package org.openapitools.api;

import java.util.List;
import java.time.OffsetDateTime;
import org.openapitools.model.User;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;


import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/user")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")public interface UserApi {

    @POST
    void createUser(@Valid @NotNull User body);

    @POST
    @Path("/createWithArray")
    void createUsersWithArrayInput(@Valid @NotNull List<User> body);

    @POST
    @Path("/createWithList")
    void createUsersWithListInput(@Valid @NotNull List<User> body);

    @DELETE
    @Path("/{username}")
    void deleteUser(@PathParam("username") String username);

    @GET
    @Path("/{username}")
    @Produces({ "application/xml", "application/json" })
    User getUserByName(@PathParam("username") String username);

    @GET
    @Path("/login")
    @Produces({ "application/xml", "application/json" })
    String loginUser(@QueryParam("username") @NotNull   String username,@QueryParam("password") @NotNull   String password);

    @GET
    @Path("/logout")
    void logoutUser();

    @PUT
    @Path("/{username}")
    void updateUser(@PathParam("username") String username,@Valid @NotNull User body);
}
