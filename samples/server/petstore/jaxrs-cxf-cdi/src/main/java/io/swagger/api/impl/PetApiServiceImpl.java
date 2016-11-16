package io.swagger.api.impl;

import io.swagger.api.*;
import java.io.File;
import io.swagger.model.ModelApiResponse;
import io.swagger.model.Pet;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import org.apache.cxf.jaxrs.model.wadl.Description;
import org.apache.cxf.jaxrs.model.wadl.DocTarget;

import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.annotations.Api;

public class PetApiServiceImpl implements PetApi {
    public void  addPet(Pet body) {
        // TODO: Implement...
        
        
    }
    
    public void  deletePet(@PathParam("petId") Long petId, @HeaderParam("api_key") String apiKey) {
        // TODO: Implement...
        
        
    }
    
    public Pet  findPetsByStatus(List<String> status) {
        // TODO: Implement...
        
        return null;
    }
    
    public Pet  findPetsByTags(List<String> tags) {
        // TODO: Implement...
        
        return null;
    }
    
    public Pet  getPetById(@PathParam("petId") Long petId) {
        // TODO: Implement...
        
        return null;
    }
    
    public void  updatePet(Pet body) {
        // TODO: Implement...
        
        
    }
    
    public void  updatePetWithForm(@PathParam("petId") Long petId, @Multipart(value = "name", required = false)  String name, @Multipart(value = "status", required = false)  String status) {
        // TODO: Implement...
        
        
    }
    
    public ModelApiResponse  uploadFile(@PathParam("petId") Long petId, @Multipart(value = "additionalMetadata", required = false)  String additionalMetadata,  @Multipart(value = "file", required = false) InputStream fileInputStream,
   @Multipart(value = "file" , required = false) Attachment fileDetail) {
        // TODO: Implement...
        
        return null;
    }
    
}

