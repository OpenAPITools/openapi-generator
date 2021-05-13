package org.openapitools.api.impl;

import org.openapitools.api.*;
import org.openapitools.model.*;
import org.jboss.resteasy.plugins.providers.multipart.MultipartFormDataInput;


import java.io.File;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.Pet;

import java.util.List;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaResteasyEapServerCodegen")
public class PetApiServiceImpl implements PetApi {
      public Response addPet(Pet body,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response deletePet(Long petId,String apiKey,SecurityContext securityContext) {
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
      public Response getPetById(Long petId,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response updatePet(Pet body,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response updatePetWithForm(Long petId,String name,String status,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response uploadFile(MultipartFormDataInput input,Long petId,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
}
