package org.openapitools.api;

import org.openapitools.api.*;
import org.openapitools.model.*;

import org.wso2.msf4j.formparam.FormDataParam;
import org.wso2.msf4j.formparam.FileInfo;

import java.util.Date;
import java.util.List;
import org.openapitools.model.User;

import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaMSF4JServerCodegen")
public abstract class UserApiService {
    public abstract Response createUser(User body
 ) throws NotFoundException;
    public abstract Response createUsersWithArrayInput(List<User> body
 ) throws NotFoundException;
    public abstract Response createUsersWithListInput(List<User> body
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
