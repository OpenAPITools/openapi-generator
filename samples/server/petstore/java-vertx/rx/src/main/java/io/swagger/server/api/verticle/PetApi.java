package io.swagger.server.api.verticle;

import java.io.File;
import io.swagger.server.api.MainApiException;
import io.swagger.server.api.model.ModelApiResponse;
import io.swagger.server.api.model.Pet;

import rx.Completable;
import rx.Single;

import java.util.List;
import java.util.Map;

public interface PetApi  {
    //addPet
    public Completable addPet(Pet body);
    
    //deletePet
    public Completable deletePet(Long petId,String apiKey);
    
    //findPetsByStatus
    public Single<List<Pet>> findPetsByStatus(List<String> status);
    
    //findPetsByTags
    public Single<List<Pet>> findPetsByTags(List<String> tags);
    
    //getPetById
    public Single<Pet> getPetById(Long petId);
    
    //updatePet
    public Completable updatePet(Pet body);
    
    //updatePetWithForm
    public Completable updatePetWithForm(Long petId,String name,String status);
    
    //uploadFile
    public Single<ModelApiResponse> uploadFile(Long petId,String additionalMetadata,File file);
    
}
