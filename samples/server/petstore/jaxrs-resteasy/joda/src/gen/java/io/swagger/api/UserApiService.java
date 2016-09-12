package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;


import io.swagger.model.User;
import java.util.List;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;


public abstract class UserApiService {
      public abstract Response createUser(User body,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response createUsersWithArrayInput(List<User> body,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response createUsersWithListInput(List<User> body,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response deleteUser(String username,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response getUserByName(String username,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response loginUser(String username,String password,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response logoutUser(SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response updateUser(String username,User body,SecurityContext securityContext)
      throws NotFoundException;
}
