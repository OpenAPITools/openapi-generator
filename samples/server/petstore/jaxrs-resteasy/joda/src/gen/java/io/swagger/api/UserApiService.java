package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;


import java.util.List;
import io.swagger.model.User;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;


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
