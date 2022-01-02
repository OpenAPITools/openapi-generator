package org.openapitools.api;

import org.openapitools.api.*;
import org.openapitools.model.*;


import java.util.List;
import java.time.OffsetDateTime;
import org.openapitools.model.User;

import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaResteasyServerCodegen")
public interface UserApiService {
      Response createUser(User body,SecurityContext securityContext)
      throws NotFoundException;
      Response createUsersWithArrayInput(List<User> body,SecurityContext securityContext)
      throws NotFoundException;
      Response createUsersWithListInput(List<User> body,SecurityContext securityContext)
      throws NotFoundException;
      Response deleteUser(String username,SecurityContext securityContext)
      throws NotFoundException;
      Response getUserByName(String username,SecurityContext securityContext)
      throws NotFoundException;
      Response loginUser(String username,String password,SecurityContext securityContext)
      throws NotFoundException;
      Response logoutUser(SecurityContext securityContext)
      throws NotFoundException;
      Response updateUser(String username,User body,SecurityContext securityContext)
      throws NotFoundException;
}
