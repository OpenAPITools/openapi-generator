package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;

import com.sun.jersey.multipart.FormDataParam;

import io.swagger.model.User;
import java.util.*;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import com.sun.jersey.core.header.FormDataContentDisposition;
import com.sun.jersey.multipart.FormDataParam;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JaxRSServerCodegen", date = "2016-01-14T21:37:36.074Z")
public abstract class UserApiService {
  
      public abstract Response createUser(User body,SecurityContext securityContext)
      throws NotFoundException;
  
      public abstract Response createUsersWithArrayInput(List<User> body,SecurityContext securityContext)
      throws NotFoundException;
  
      public abstract Response createUsersWithListInput(List<User> body,SecurityContext securityContext)
      throws NotFoundException;
  
      public abstract Response loginUser(String username,String password,SecurityContext securityContext)
      throws NotFoundException;
  
      public abstract Response logoutUser(SecurityContext securityContext)
      throws NotFoundException;
  
      public abstract Response getUserByName(String username,SecurityContext securityContext)
      throws NotFoundException;
  
      public abstract Response updateUser(String username,User body,SecurityContext securityContext)
      throws NotFoundException;
  
      public abstract Response deleteUser(String username,SecurityContext securityContext)
      throws NotFoundException;
  
}
