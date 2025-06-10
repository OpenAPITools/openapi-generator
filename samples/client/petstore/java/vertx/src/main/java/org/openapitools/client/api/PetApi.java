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

    void addPet(@javax.annotation.Nonnull Pet pet, Handler<AsyncResult<Void>> handler);

    void addPet(@javax.annotation.Nonnull Pet pet, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void deletePet(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String apiKey, Handler<AsyncResult<Void>> handler);

    void deletePet(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String apiKey, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void findPetsByStatus(@javax.annotation.Nonnull List<String> status, Handler<AsyncResult<List<Pet>>> handler);

    void findPetsByStatus(@javax.annotation.Nonnull List<String> status, ApiClient.AuthInfo authInfo, Handler<AsyncResult<List<Pet>>> handler);

    @Deprecated
    void findPetsByTags(@javax.annotation.Nonnull Set<String> tags, Handler<AsyncResult<Set<Pet>>> handler);

    @Deprecated
    void findPetsByTags(@javax.annotation.Nonnull Set<String> tags, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Set<Pet>>> handler);

    void getPetById(@javax.annotation.Nonnull Long petId, Handler<AsyncResult<Pet>> handler);

    void getPetById(@javax.annotation.Nonnull Long petId, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Pet>> handler);

    void updatePet(@javax.annotation.Nonnull Pet pet, Handler<AsyncResult<Void>> handler);

    void updatePet(@javax.annotation.Nonnull Pet pet, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void updatePetWithForm(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String name, @javax.annotation.Nullable String status, Handler<AsyncResult<Void>> handler);

    void updatePetWithForm(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String name, @javax.annotation.Nullable String status, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void uploadFile(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String additionalMetadata, @javax.annotation.Nullable AsyncFile _file, Handler<AsyncResult<ModelApiResponse>> handler);

    void uploadFile(@javax.annotation.Nonnull Long petId, @javax.annotation.Nullable String additionalMetadata, @javax.annotation.Nullable AsyncFile _file, ApiClient.AuthInfo authInfo, Handler<AsyncResult<ModelApiResponse>> handler);

    void uploadFileWithRequiredFile(@javax.annotation.Nonnull Long petId, @javax.annotation.Nonnull AsyncFile requiredFile, @javax.annotation.Nullable String additionalMetadata, Handler<AsyncResult<ModelApiResponse>> handler);

    void uploadFileWithRequiredFile(@javax.annotation.Nonnull Long petId, @javax.annotation.Nonnull AsyncFile requiredFile, @javax.annotation.Nullable String additionalMetadata, ApiClient.AuthInfo authInfo, Handler<AsyncResult<ModelApiResponse>> handler);

}
