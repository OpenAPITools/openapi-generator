package org.openapitools.vertxweb.server.api;

import io.vertx.ext.web.FileUpload;
import org.openapitools.vertxweb.server.model.ModelApiResponse;
import org.openapitools.vertxweb.server.model.Pet;

import org.openapitools.vertxweb.server.ApiResponse;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.handler.impl.HttpStatusException;

import java.util.List;
import java.util.Map;

// Implement this class

public class PetApiImpl implements PetApi {
    public Future<ApiResponse<Pet>> addPet(Pet pet) {
        return Future.failedFuture(new HttpStatusException(501));
    }

    public Future<ApiResponse<Void>> deletePet(Long petId, String apiKey) {
        return Future.failedFuture(new HttpStatusException(501));
    }

    public Future<ApiResponse<List<Pet>>> findPetsByStatus(List<String> status) {
        return Future.failedFuture(new HttpStatusException(501));
    }

    public Future<ApiResponse<List<Pet>>> findPetsByTags(List<String> tags) {
        return Future.failedFuture(new HttpStatusException(501));
    }

    public Future<ApiResponse<Pet>> getPetById(Long petId) {
        return Future.failedFuture(new HttpStatusException(501));
    }

    public Future<ApiResponse<Pet>> updatePet(Pet pet) {
        return Future.failedFuture(new HttpStatusException(501));
    }

    public Future<ApiResponse<Void>> updatePetWithForm(Long petId, JsonObject formBody) {
        return Future.failedFuture(new HttpStatusException(501));
    }

    public Future<ApiResponse<ModelApiResponse>> uploadFile(Long petId, FileUpload file) {
        return Future.failedFuture(new HttpStatusException(501));
    }

}
