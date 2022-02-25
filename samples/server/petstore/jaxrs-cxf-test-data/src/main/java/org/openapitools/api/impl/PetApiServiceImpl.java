package org.openapitools.api.impl;

import org.openapitools.api.*;
import org.openapitools.model.Category;
import java.io.File;
import java.util.List;
import java.util.Map;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.Pet;
import java.util.Set;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import java.io.File;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.openapitools.codegen.utils.JsonCache;
import org.openapitools.codegen.utils.JsonCache.CacheException;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import org.apache.cxf.jaxrs.model.wadl.Description;
import org.apache.cxf.jaxrs.model.wadl.DocTarget;

import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.annotations.Api;

/**
 * OpenAPI Petstore
 *
 * <p>This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\
 *
 */
public class PetApiServiceImpl implements PetApi {
    private JsonCache cache;

    {
        try {
            File cacheFile = new File(System.getProperty("jaxrs.test.server.json",
                    "/Users/williamcheng/Code/openapi-generator2/samples/server/petstore/jaxrs-cxf-test-data/src/main/resources/test-data.json"));
            cache = JsonCache.Factory.instance.get("test-data").load(cacheFile).child("/org.openapitools.api/PetApi");
        } catch (CacheException e) {
            e.printStackTrace();
        }
    }

    /**
     * Add a new pet to the store
     *
     */
    @Override
    public void addPet(Pet body) {

    }

    /**
     * Deletes a pet
     *
     */
    @Override
    public void deletePet(Long petId, String apiKey) {

    }

    /**
     * Finds Pets by status
     *
     * Multiple status values can be provided with comma separated strings
     *
     */
    @Override
    public List<Pet> findPetsByStatus(List<String> status) {
        try {
            List<Pet> response = cache.getObjects("/findPetsByStatus/response", Pet.class);
            return response;
        } catch (CacheException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Finds Pets by tags
     *
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     *
     */
    @Override
    public Set<Pet> findPetsByTags(Set<String> tags) {
        try {
            Set<Pet> response = cache.getObjects("/findPetsByTags/response", Pet.class);
            return response;
        } catch (CacheException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Find pet by ID
     *
     * Returns a single pet
     *
     */
    @Override
    public Pet getPetById(Long petId) {
        try {
            Pet response = cache.getObject("/getPetById/response", Pet.class);
            return response;
        } catch (CacheException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Update an existing pet
     *
     */
    @Override
    public void updatePet(Pet body) {

    }

    /**
     * Updates a pet in the store with form data
     *
     */
    @Override
    public void updatePetWithForm(Long petId, String name, String status) {

    }

    /**
     * uploads an image
     *
     */
    @Override
    public ModelApiResponse uploadFile(Long petId, String additionalMetadata,  Attachment _fileDetail) {
        try {
            ModelApiResponse response = cache.getObject("/uploadFile/response", ModelApiResponse.class);
            return response;
        } catch (CacheException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * uploads an image (required)
     *
     */
    @Override
    public ModelApiResponse uploadFileWithRequiredFile(Long petId,  Attachment requiredFileDetail, String additionalMetadata) {
        try {
            ModelApiResponse response = cache.getObject("/uploadFileWithRequiredFile/response", ModelApiResponse.class);
            return response;
        } catch (CacheException e) {
            throw new RuntimeException(e);
        }
    }

}
