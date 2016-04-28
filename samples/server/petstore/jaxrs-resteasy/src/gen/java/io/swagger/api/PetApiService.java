package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;
import org.jboss.resteasy.plugins.providers.multipart.MultipartFormDataInput;


import io.swagger.model.Pet;
import java.io.File;
import io.swagger.model.ModelApiResponse;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaResteasyServerCodegen", date = "2016-04-29T00:20:47.240+08:00")
public abstract class PetApiService {
      public abstract Response addPet(Pet body,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response deletePet(Long petId,String apiKey,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response findPetsByStatus(List<String> status,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response findPetsByTags(List<String> tags,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response getPetById(Long petId,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response updatePet(Pet body,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response updatePetWithForm(Long petId,String name,String status,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response uploadFile(MultipartFormDataInput input,Long petId,SecurityContext securityContext)
      throws NotFoundException;
}
