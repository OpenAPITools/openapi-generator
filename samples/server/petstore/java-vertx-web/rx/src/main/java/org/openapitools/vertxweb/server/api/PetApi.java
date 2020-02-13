package org.openapitools.vertxweb.server.api;

import io.vertx.ext.web.FileUpload;
import org.openapitools.vertxweb.server.model.ModelApiResponse;
import org.openapitools.vertxweb.server.model.Pet;

import org.openapitools.vertxweb.server.ApiResponse;

import io.reactivex.Single;

import java.util.List;
import java.util.Map;

public interface PetApi  {
    Single<ApiResponse<Void>> addPet(Pet pet);
    Single<ApiResponse<Void>> deletePet(Long petId,String apiKey);
    Single<ApiResponse<List<Pet>>> findPetsByStatus(List<String> status);
    Single<ApiResponse<List<Pet>>> findPetsByTags(List<String> tags);
    Single<ApiResponse<Pet>> getPetById(Long petId);
    Single<ApiResponse<Void>> updatePet(Pet pet);
    Single<ApiResponse<Void>> updatePetWithForm(Long petId,String name,String status);
    Single<ApiResponse<ModelApiResponse>> uploadFile(Long petId,String additionalMetadata,FileUpload file);
}
