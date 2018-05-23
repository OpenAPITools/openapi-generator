package org.openapitools.api.impl;

import org.openapitools.api.*;
import org.openapitools.model.*;
import org.jboss.resteasy.plugins.providers.multipart.MultipartFormDataInput;


import java.io.File;
import java.util.List;
import java.util.Map;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.Order;
import org.openapitools.model.Pet;
import org.openapitools.model.User;

import java.util.List;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;


public class DefaultApiServiceImpl implements DefaultApi {
      public Response addPet(Pet pet,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response createUser(User user,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response createUsersWithArrayInput(List<User> user,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response createUsersWithListInput(List<User> user,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response deleteOrder(String orderId,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response deletePet(Long petId,String apiKey,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response deleteUser(String username,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response findPetsByStatus(List<String> status,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response findPetsByTags(List<String> tags,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response getInventory(SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response getOrderById(Long orderId,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response getPetById(Long petId,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response getUserByName(String username,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response loginUser(String username,String password,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response logoutUser(SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response placeOrder(Order order,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response updatePet(Pet pet,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response updatePetWithForm(Long petId,String name,String status,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response updateUser(String username,User user,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response uploadFile(MultipartFormDataInput input,Long petId,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
}
