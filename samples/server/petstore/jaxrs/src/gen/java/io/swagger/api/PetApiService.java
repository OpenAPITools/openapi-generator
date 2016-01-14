package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;

import com.sun.jersey.multipart.FormDataParam;

import io.swagger.model.Pet;
import java.io.File;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import com.sun.jersey.core.header.FormDataContentDisposition;
import com.sun.jersey.multipart.FormDataParam;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JaxRSServerCodegen", date = "2016-01-14T21:37:36.074Z")
public abstract class PetApiService {
  
      public abstract Response updatePet(Pet body,SecurityContext securityContext)
      throws NotFoundException;
  
      public abstract Response addPet(Pet body,SecurityContext securityContext)
      throws NotFoundException;
  
      public abstract Response findPetsByStatus(List<String> status,SecurityContext securityContext)
      throws NotFoundException;
  
      public abstract Response findPetsByTags(List<String> tags,SecurityContext securityContext)
      throws NotFoundException;
  
      public abstract Response getPetById(Long petId,SecurityContext securityContext)
      throws NotFoundException;
  
      public abstract Response updatePetWithForm(String petId,String name,String status,SecurityContext securityContext)
      throws NotFoundException;
  
      public abstract Response deletePet(Long petId,String apiKey,SecurityContext securityContext)
      throws NotFoundException;
  
      public abstract Response uploadFile(Long petId,String additionalMetadata,InputStream inputStream, FormDataContentDisposition fileDetail,SecurityContext securityContext)
      throws NotFoundException;
  
      public abstract Response getPetByIdWithByteArray(Long petId,SecurityContext securityContext)
      throws NotFoundException;
  
}
