package org.openapitools.api;

import org.openapitools.api.*;
import org.openapitools.model.*;

import java.util.List;
import org.openapitools.model.User;

import org.openapitools.api.NotFoundException;

import javax.ws.rs.core.Response;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaMSF4JServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public abstract class UserApiService {
    public abstract Response createUser(User body
 ) throws NotFoundException;
    public abstract Response createUsersWithArrayInput(List<@Valid User> body
 ) throws NotFoundException;
    public abstract Response createUsersWithListInput(List<@Valid User> body
 ) throws NotFoundException;
    public abstract Response deleteUser(String username
 ) throws NotFoundException;
    public abstract Response getUserByName(String username
 ) throws NotFoundException;
    public abstract Response loginUser(String username
 ,String password
 ) throws NotFoundException;
    public abstract Response logoutUser() throws NotFoundException;
    public abstract Response updateUser(String username
 ,User body
 ) throws NotFoundException;
}
