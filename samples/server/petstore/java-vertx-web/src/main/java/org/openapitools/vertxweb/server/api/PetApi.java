package org.openapitools.vertxweb.server.api;

import io.vertx.ext.web.FileUpload;
import org.openapitools.vertxweb.server.model.ModelApiResponse;
import org.openapitools.vertxweb.server.model.Pet;

import org.openapitools.vertxweb.server.ApiResponse;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

import java.util.List;
import java.util.Map;

public interface PetApi  {
    Future<ApiResponse<Pet>> addPet(Pet pet);
    Future<ApiResponse<Void>> deletePet(Long petId, String apiKey);
    Future<ApiResponse<List<Pet>>> findPetsByStatus(List<String> status);
    Future<ApiResponse<List<Pet>>> findPetsByTags(List<String> tags);
    Future<ApiResponse<Pet>> getPetById(Long petId);
    Future<ApiResponse<Pet>> updatePet(Pet pet);
    Future<ApiResponse<Void>> updatePetWithForm(Long petId, JsonObject formBody);
    Future<ApiResponse<ModelApiResponse>> uploadFile(Long petId, FileUpload file);
}
