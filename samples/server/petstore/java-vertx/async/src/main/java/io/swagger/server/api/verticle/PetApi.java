package io.swagger.server.api.verticle;

import java.io.File;
import io.swagger.server.api.MainApiException;
import io.swagger.server.api.model.ModelApiResponse;
import io.swagger.server.api.model.Pet;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;

import java.util.List;
import java.util.Map;

public interface PetApi  {
    //addPet
    void addPet(Pet body, Handler<AsyncResult<Void>> handler);
    
    //deletePet
    void deletePet(Long petId, String apiKey, Handler<AsyncResult<Void>> handler);
    
    //findPetsByStatus
    void findPetsByStatus(List<String> status, Handler<AsyncResult<List<Pet>>> handler);
    
    //findPetsByTags
    void findPetsByTags(List<String> tags, Handler<AsyncResult<List<Pet>>> handler);
    
    //getPetById
    void getPetById(Long petId, Handler<AsyncResult<Pet>> handler);
    
    //updatePet
    void updatePet(Pet body, Handler<AsyncResult<Void>> handler);
    
    //updatePetWithForm
    void updatePetWithForm(Long petId, String name, String status, Handler<AsyncResult<Void>> handler);
    
    //uploadFile
    void uploadFile(Long petId, String additionalMetadata, File file, Handler<AsyncResult<ModelApiResponse>> handler);
    
}
