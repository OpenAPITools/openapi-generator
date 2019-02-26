package org.openapitools.api;

import org.openapitools.api.*;
import org.openapitools.model.*;

import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import java.io.File;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.Pet;

import java.util.List;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;


public interface PetApiService {
      public Response addPet(Pet body, SecurityContext securityContext);
      public Response deletePet(Long petId, String apiKey, SecurityContext securityContext);
      public Response findPetsByStatus(List<String> status, SecurityContext securityContext);
      public Response findPetsByTags(List<String> tags, SecurityContext securityContext);
      public Response getPetById(Long petId, SecurityContext securityContext);
      public Response updatePet(Pet body, SecurityContext securityContext);
      public Response updatePetWithForm(Long petId, String name, String status, SecurityContext securityContext);
      public Response uploadFile(Long petId, String additionalMetadata, InputStream fileInputStream, Attachment fileDetail, SecurityContext securityContext);
}
