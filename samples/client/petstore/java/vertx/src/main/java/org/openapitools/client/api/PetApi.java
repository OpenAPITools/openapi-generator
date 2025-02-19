package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import io.vertx.core.file.AsyncFile;
import org.openapitools.client.model.ModelApiResponse;
import org.openapitools.client.model.Pet;
import java.util.Set;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface PetApi {

    void addPet(Pet pet, Handler<AsyncResult<Void>> handler);

    void addPet(Pet pet, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void deletePet(Long petId, String apiKey, Handler<AsyncResult<Void>> handler);

    void deletePet(Long petId, String apiKey, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void findPetsByStatus(List<String> status, Handler<AsyncResult<List<Pet>>> handler);

    void findPetsByStatus(List<String> status, ApiClient.AuthInfo authInfo, Handler<AsyncResult<List<Pet>>> handler);

    @Deprecated
    void findPetsByTags(Set<String> tags, Handler<AsyncResult<Set<Pet>>> handler);

    @Deprecated
    void findPetsByTags(Set<String> tags, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Set<Pet>>> handler);

    void getPetById(Long petId, Handler<AsyncResult<Pet>> handler);

    void getPetById(Long petId, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Pet>> handler);

    void updatePet(Pet pet, Handler<AsyncResult<Void>> handler);

    void updatePet(Pet pet, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void updatePetWithForm(Long petId, String name, String status, Handler<AsyncResult<Void>> handler);

    void updatePetWithForm(Long petId, String name, String status, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void uploadFile(Long petId, String additionalMetadata, AsyncFile _file, Handler<AsyncResult<ModelApiResponse>> handler);

    void uploadFile(Long petId, String additionalMetadata, AsyncFile _file, ApiClient.AuthInfo authInfo, Handler<AsyncResult<ModelApiResponse>> handler);

    void uploadFileWithRequiredFile(Long petId, AsyncFile requiredFile, String additionalMetadata, Handler<AsyncResult<ModelApiResponse>> handler);

    void uploadFileWithRequiredFile(Long petId, AsyncFile requiredFile, String additionalMetadata, ApiClient.AuthInfo authInfo, Handler<AsyncResult<ModelApiResponse>> handler);

}
