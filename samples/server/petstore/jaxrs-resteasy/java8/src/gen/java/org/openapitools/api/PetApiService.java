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

import javax.validation.constraints.*;
import javax.validation.Valid;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaResteasyServerCodegen", comments = "Generator version: 7.18.0-SNAPSHOT")
public interface PetApiService {
    Response addPet(
        
        Pet body,
        SecurityContext securityContext
    ) throws NotFoundException;

    default Response addPet(
        
        Pet body, 
        String context
    ) throws NotFoundException {
        return addPet(
            
            body,
            (SecurityContext)null
        );
    }
    Response deletePet(
        
        Long petId,
        String apiKey,
        SecurityContext securityContext
    ) throws NotFoundException;

    default Response deletePet(
        
        Long petId, 
        String apiKey, 
        String context
    ) throws NotFoundException {
        return deletePet(
            
            petId,apiKey,
            (SecurityContext)null
        );
    }
    Response findPetsByStatus(
        
        List<String> status,
        SecurityContext securityContext
    ) throws NotFoundException;

    default Response findPetsByStatus(
        
        List<String> status, 
        String context
    ) throws NotFoundException {
        return findPetsByStatus(
            
            status,
            (SecurityContext)null
        );
    }
    Response findPetsByTags(
        
        List<String> tags,
        SecurityContext securityContext
    ) throws NotFoundException;

    default Response findPetsByTags(
        
        List<String> tags, 
        String context
    ) throws NotFoundException {
        return findPetsByTags(
            
            tags,
            (SecurityContext)null
        );
    }
    Response getPetById(
        
        Long petId,
        SecurityContext securityContext
    ) throws NotFoundException;

    default Response getPetById(
        
        Long petId, 
        String context
    ) throws NotFoundException {
        return getPetById(
            
            petId,
            (SecurityContext)null
        );
    }
    Response updatePet(
        
        Pet body,
        SecurityContext securityContext
    ) throws NotFoundException;

    default Response updatePet(
        
        Pet body, 
        String context
    ) throws NotFoundException {
        return updatePet(
            
            body,
            (SecurityContext)null
        );
    }
    Response updatePetWithForm(
        
        Long petId,
        String name,
        String status,
        SecurityContext securityContext
    ) throws NotFoundException;

    default Response updatePetWithForm(
        
        Long petId, 
        String name, 
        String status, 
        String context
    ) throws NotFoundException {
        return updatePetWithForm(
            
            petId,name,status,
            (SecurityContext)null
        );
    }
    Response uploadFile(
        MultipartFormDataInput input,
        Long petId,
        
        
        SecurityContext securityContext
    ) throws NotFoundException;

    default Response uploadFile(
        MultipartFormDataInput input,
        Long petId, 
        
        
        String context
    ) throws NotFoundException {
        return uploadFile(
            input,
            petId,additionalMetadata,_file,
            (SecurityContext)null
        );
    }
}
