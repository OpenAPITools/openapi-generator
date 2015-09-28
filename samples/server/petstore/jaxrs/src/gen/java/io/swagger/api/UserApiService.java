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

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JaxRSServerCodegen", date = "2015-08-23T23:29:16.812-07:00")
public abstract class UserApiService {
  
      public abstract Response createUser(User body)
      throws NotFoundException;
  
      public abstract Response createUsersWithArrayInput(List<User> body)
      throws NotFoundException;
  
      public abstract Response createUsersWithListInput(List<User> body)
      throws NotFoundException;
  
      public abstract Response loginUser(String username,String password)
      throws NotFoundException;
  
      public abstract Response logoutUser()
      throws NotFoundException;
  
      public abstract Response getUserByName(String username)
      throws NotFoundException;
  
      public abstract Response updateUser(String username,User body)
      throws NotFoundException;
  
      public abstract Response deleteUser(String username)
      throws NotFoundException;
  
}
