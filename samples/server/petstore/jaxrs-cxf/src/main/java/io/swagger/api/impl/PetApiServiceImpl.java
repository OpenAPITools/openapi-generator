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

/**
 * Swagger Petstore
 *
 * <p>This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\
 *
 */
public class PetApiServiceImpl implements PetApi {
    /**
     * Add a new pet to the store
     *
     * 
     *
     */
    public void addPet(Pet body) {
        // TODO: Implement...
        
        
    }
    
    /**
     * Deletes a pet
     *
     * 
     *
     */
    public void deletePet(Long petId, String apiKey) {
        // TODO: Implement...
        
        
    }
    
    /**
     * Finds Pets by status
     *
     * Multiple status values can be provided with comma separated strings
     *
     */
    public List<Pet> findPetsByStatus(List<String> status) {
        // TODO: Implement...
        
        return null;
    }
    
    /**
     * Finds Pets by tags
     *
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     *
     */
    public List<Pet> findPetsByTags(List<String> tags) {
        // TODO: Implement...
        
        return null;
    }
    
    /**
     * Find pet by ID
     *
     * Returns a single pet
     *
     */
    public Pet getPetById(Long petId) {
        // TODO: Implement...
        
        return null;
    }
    
    /**
     * Update an existing pet
     *
     * 
     *
     */
    public void updatePet(Pet body) {
        // TODO: Implement...
        
        
    }
    
    /**
     * Updates a pet in the store with form data
     *
     * 
     *
     */
    public void updatePetWithForm(Long petId, String name, String status) {
        // TODO: Implement...
        
        
    }
    
    /**
     * uploads an image
     *
     * 
     *
     */
    public ModelApiResponse uploadFile(Long petId, String additionalMetadata,  Attachment fileDetail) {
        // TODO: Implement...
        
        return null;
    }
    
}

