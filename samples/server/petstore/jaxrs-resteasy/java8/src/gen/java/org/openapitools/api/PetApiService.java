package org.openapitools.api;

import org.openapitools.api.*;
import org.openapitools.model.*;
import org.jboss.resteasy.plugins.providers.multipart.MultipartFormDataInput;


import java.io.File;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.Pet;

import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaResteasyServerCodegen")
public interface PetApiService {
      Response addPet(Pet body,SecurityContext securityContext)
      throws NotFoundException;
      Response deletePet(Long petId,String apiKey,SecurityContext securityContext)
      throws NotFoundException;
      Response findPetsByStatus(List<String> status,SecurityContext securityContext)
      throws NotFoundException;
      Response findPetsByTags(List<String> tags,SecurityContext securityContext)
      throws NotFoundException;
      Response getPetById(Long petId,SecurityContext securityContext)
      throws NotFoundException;
      Response updatePet(Pet body,SecurityContext securityContext)
      throws NotFoundException;
      Response updatePetWithForm(Long petId,String name,String status,SecurityContext securityContext)
      throws NotFoundException;
      Response uploadFile(MultipartFormDataInput input,Long petId,SecurityContext securityContext)
      throws NotFoundException;
}
