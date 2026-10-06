package org.openapitools.api;

import org.openapitools.api.*;
import org.openapitools.model.*;

import org.openapitools.model.User;

import java.util.List;

import javax.validation.constraints.*;
import javax.validation.Valid;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSCXFCDIServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public interface UserApiService {
      public Response createUser(User user, SecurityContext securityContext);
      public Response createUsersWithArrayInput(List<@Valid User> user, SecurityContext securityContext);
      public Response createUsersWithListInput(List<@Valid User> user, SecurityContext securityContext);
      public Response deleteUser(String username, SecurityContext securityContext);
      public Response getUserByName(String username, SecurityContext securityContext);
      public Response loginUser(String username, String password, SecurityContext securityContext);
      public Response logoutUser(SecurityContext securityContext);
      public Response updateUser(String username, User user, SecurityContext securityContext);
}
