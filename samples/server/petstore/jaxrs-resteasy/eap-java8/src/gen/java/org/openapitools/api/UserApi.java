package org.openapitools.api;

import java.util.List;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

import org.openapitools.model.User;

@Path("/user")


@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaResteasyEapServerCodegen")
public interface UserApi  {

    @POST Response createUser( @NotNull @Valid User body,@Context SecurityContext securityContext);
    @POST
    @Path("/createWithArray") Response createUsersWithArrayInput( @NotNull @Valid List<User> body,@Context SecurityContext securityContext);
    @POST
    @Path("/createWithList") Response createUsersWithListInput( @NotNull @Valid List<User> body,@Context SecurityContext securityContext);
    @DELETE
    @Path("/{username}") Response deleteUser( @PathParam("username") String username,@Context SecurityContext securityContext);
    @GET
    @Path("/{username}")

    @Produces({ "application/xml", "application/json" }) Response getUserByName( @PathParam("username") String username,@Context SecurityContext securityContext);
    @GET
    @Path("/login")

    @Produces({ "application/xml", "application/json" }) Response loginUser( @NotNull  @QueryParam("username") String username, @NotNull  @QueryParam("password") String password,@Context SecurityContext securityContext);
    @GET
    @Path("/logout") Response logoutUser(@Context SecurityContext securityContext);
    @PUT
    @Path("/{username}") Response updateUser( @PathParam("username") String username, @NotNull @Valid User body,@Context SecurityContext securityContext);
}
