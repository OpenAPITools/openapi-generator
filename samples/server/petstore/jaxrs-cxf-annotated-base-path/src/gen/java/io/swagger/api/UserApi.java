package io.swagger.api;

import java.util.List;
import io.swagger.model.User;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.MediaType;
import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.jaxrs.PATCH;
import javax.validation.constraints.*;

@Path("/v2")
@Api(value = "/", description = "")
public interface UserApi  {

    @POST
    @Path("/user")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Create user", tags={ "user",  })
    public void createUser(User body);

    @POST
    @Path("/user/createWithArray")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Creates list of users with given input array", tags={ "user",  })
    public void createUsersWithArrayInput(List<User> body);

    @POST
    @Path("/user/createWithList")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Creates list of users with given input array", tags={ "user",  })
    public void createUsersWithListInput(List<User> body);

    @DELETE
    @Path("/user/{username}")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Delete user", tags={ "user",  })
    public void deleteUser(@PathParam("username") String username);

    @GET
    @Path("/user/{username}")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Get user by user name", tags={ "user",  })
    public User getUserByName(@PathParam("username") String username);

    @GET
    @Path("/user/login")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Logs user into the system", tags={ "user",  })
    public String loginUser(@QueryParam("username") @NotNull String username, @QueryParam("password") @NotNull String password);

    @GET
    @Path("/user/logout")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Logs out current logged in user session", tags={ "user",  })
    public void logoutUser();

    @PUT
    @Path("/user/{username}")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Updated user", tags={ "user" })
    public void updateUser(@PathParam("username") String username, User body);
}

