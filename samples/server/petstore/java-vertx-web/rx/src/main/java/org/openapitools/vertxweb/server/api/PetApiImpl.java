package org.openapitools.vertxweb.server.api;

import io.vertx.ext.web.FileUpload;
import org.openapitools.vertxweb.server.model.ModelApiResponse;
import org.openapitools.vertxweb.server.model.Pet;

import org.openapitools.vertxweb.server.ApiResponse;
import org.openapitools.vertxweb.server.ApiException;

import io.reactivex.Single;

import java.util.List;
import java.util.Map;

// Implement this class

public class PetApiImpl implements PetApi {
    public Single<ApiResponse<Void>> addPet(Pet pet) {
        return Single.error(new ApiException("Not Implemented").setStatusCode(501));
    }

    public Single<ApiResponse<Void>> deletePet(Long petId,String apiKey) {
        return Single.error(new ApiException("Not Implemented").setStatusCode(501));
    }

    public Single<ApiResponse<List<Pet>>> findPetsByStatus(List<String> status) {
        return Single.error(new ApiException("Not Implemented").setStatusCode(501));
    }

    public Single<ApiResponse<List<Pet>>> findPetsByTags(List<String> tags) {
        return Single.error(new ApiException("Not Implemented").setStatusCode(501));
    }

    public Single<ApiResponse<Pet>> getPetById(Long petId) {
        return Single.error(new ApiException("Not Implemented").setStatusCode(501));
    }

    public Single<ApiResponse<Void>> updatePet(Pet pet) {
        return Single.error(new ApiException("Not Implemented").setStatusCode(501));
    }

    public Single<ApiResponse<Void>> updatePetWithForm(Long petId,String name,String status) {
        return Single.error(new ApiException("Not Implemented").setStatusCode(501));
    }

    public Single<ApiResponse<ModelApiResponse>> uploadFile(Long petId,String additionalMetadata,FileUpload file) {
        return Single.error(new ApiException("Not Implemented").setStatusCode(501));
    }

}
