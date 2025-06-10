package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import io.vertx.core.file.AsyncFile;
import org.openapitools.client.model.ModelApiResponse;
import org.openapitools.client.model.Pet;
import java.util.Set;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface PetApi {

    void addPet(@javax.annotation.Nonnull Pet pet, Handler<AsyncResult<Void>> handler);

    default Future<Void> addPet(@javax.annotation.Nonnull Pet pet){
        Promise<Void> promise = Promise.promise();
        addPet(pet, promise);
        return promise.future();
    }

    void addPet(@javax.annotation.Nonnull Pet pet, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> addPet(@javax.annotation.Nonnull Pet pet, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        addPet(pet, authInfo, promise);
        return promise.future();
    }

    void deletePet(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String apiKey, Handler<AsyncResult<Void>> handler);

    default Future<Void> deletePet(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String apiKey){
        Promise<Void> promise = Promise.promise();
        deletePet(petId, apiKey, promise);
        return promise.future();
    }

    void deletePet(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String apiKey, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> deletePet(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String apiKey, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        deletePet(petId, apiKey, authInfo, promise);
        return promise.future();
    }

    void findPetsByStatus(@javax.annotation.Nonnull List<String> status, Handler<AsyncResult<List<Pet>>> handler);

    default Future<List<Pet>> findPetsByStatus(@javax.annotation.Nonnull List<String> status){
        Promise<List<Pet>> promise = Promise.promise();
        findPetsByStatus(status, promise);
        return promise.future();
    }

    void findPetsByStatus(@javax.annotation.Nonnull List<String> status, ApiClient.AuthInfo authInfo, Handler<AsyncResult<List<Pet>>> handler);

    default Future<List<Pet>> findPetsByStatus(@javax.annotation.Nonnull List<String> status, ApiClient.AuthInfo authInfo){
        Promise<List<Pet>> promise = Promise.promise();
        findPetsByStatus(status, authInfo, promise);
        return promise.future();
    }

    @Deprecated
    void findPetsByTags(@javax.annotation.Nonnull Set<String> tags, Handler<AsyncResult<Set<Pet>>> handler);

    @Deprecated
    default Future<Set<Pet>> findPetsByTags(@javax.annotation.Nonnull Set<String> tags){
        Promise<Set<Pet>> promise = Promise.promise();
        findPetsByTags(tags, promise);
        return promise.future();
    }

    @Deprecated
    void findPetsByTags(@javax.annotation.Nonnull Set<String> tags, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Set<Pet>>> handler);

    @Deprecated
    default Future<Set<Pet>> findPetsByTags(@javax.annotation.Nonnull Set<String> tags, ApiClient.AuthInfo authInfo){
        Promise<Set<Pet>> promise = Promise.promise();
        findPetsByTags(tags, authInfo, promise);
        return promise.future();
    }

    void getPetById(@javax.annotation.Nonnull Long petId, Handler<AsyncResult<Pet>> handler);

    default Future<Pet> getPetById(@javax.annotation.Nonnull Long petId){
        Promise<Pet> promise = Promise.promise();
        getPetById(petId, promise);
        return promise.future();
    }

    void getPetById(@javax.annotation.Nonnull Long petId, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Pet>> handler);

    default Future<Pet> getPetById(@javax.annotation.Nonnull Long petId, ApiClient.AuthInfo authInfo){
        Promise<Pet> promise = Promise.promise();
        getPetById(petId, authInfo, promise);
        return promise.future();
    }

    void updatePet(@javax.annotation.Nonnull Pet pet, Handler<AsyncResult<Void>> handler);

    default Future<Void> updatePet(@javax.annotation.Nonnull Pet pet){
        Promise<Void> promise = Promise.promise();
        updatePet(pet, promise);
        return promise.future();
    }

    void updatePet(@javax.annotation.Nonnull Pet pet, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> updatePet(@javax.annotation.Nonnull Pet pet, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        updatePet(pet, authInfo, promise);
        return promise.future();
    }

    void updatePetWithForm(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String name, @javax.annotation.Nullable String status, Handler<AsyncResult<Void>> handler);

    default Future<Void> updatePetWithForm(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String name, @javax.annotation.Nullable String status){
        Promise<Void> promise = Promise.promise();
        updatePetWithForm(petId, name, status, promise);
        return promise.future();
    }

    void updatePetWithForm(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String name, @javax.annotation.Nullable String status, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> updatePetWithForm(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String name, @javax.annotation.Nullable String status, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        updatePetWithForm(petId, name, status, authInfo, promise);
        return promise.future();
    }

    void uploadFile(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String additionalMetadata, @javax.annotation.Nullable AsyncFile _file, Handler<AsyncResult<ModelApiResponse>> handler);

    default Future<ModelApiResponse> uploadFile(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String additionalMetadata, @javax.annotation.Nullable AsyncFile _file){
        Promise<ModelApiResponse> promise = Promise.promise();
        uploadFile(petId, additionalMetadata, _file, promise);
        return promise.future();
    }

    void uploadFile(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String additionalMetadata, @javax.annotation.Nullable AsyncFile _file, ApiClient.AuthInfo authInfo, Handler<AsyncResult<ModelApiResponse>> handler);

    default Future<ModelApiResponse> uploadFile(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String additionalMetadata, @javax.annotation.Nullable AsyncFile _file, ApiClient.AuthInfo authInfo){
        Promise<ModelApiResponse> promise = Promise.promise();
        uploadFile(petId, additionalMetadata, _file, authInfo, promise);
        return promise.future();
    }

    void uploadFileWithRequiredFile(@javax.annotation.Nonnull Long petId, @javax.annotation.Nonnull AsyncFile requiredFile, @javax.annotation.Nullable String additionalMetadata, Handler<AsyncResult<ModelApiResponse>> handler);

    default Future<ModelApiResponse> uploadFileWithRequiredFile(@javax.annotation.Nonnull Long petId, @javax.annotation.Nonnull AsyncFile requiredFile, @javax.annotation.Nullable String additionalMetadata){
        Promise<ModelApiResponse> promise = Promise.promise();
        uploadFileWithRequiredFile(petId, requiredFile, additionalMetadata, promise);
        return promise.future();
    }

    void uploadFileWithRequiredFile(@javax.annotation.Nonnull Long petId, @javax.annotation.Nonnull AsyncFile requiredFile, @javax.annotation.Nullable String additionalMetadata, ApiClient.AuthInfo authInfo, Handler<AsyncResult<ModelApiResponse>> handler);

    default Future<ModelApiResponse> uploadFileWithRequiredFile(@javax.annotation.Nonnull Long petId, @javax.annotation.Nonnull AsyncFile requiredFile, @javax.annotation.Nullable String additionalMetadata, ApiClient.AuthInfo authInfo){
        Promise<ModelApiResponse> promise = Promise.promise();
        uploadFileWithRequiredFile(petId, requiredFile, additionalMetadata, authInfo, promise);
        return promise.future();
    }

}
