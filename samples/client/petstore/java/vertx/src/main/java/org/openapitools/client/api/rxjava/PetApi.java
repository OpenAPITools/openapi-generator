package org.openapitools.client.api.rxjava;

import io.vertx.core.file.AsyncFile;
import org.openapitools.client.model.ModelApiResponse;
import org.openapitools.client.model.Pet;
import java.util.Set;

import java.util.*;

import rx.Single;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;


public class PetApi {

	private final org.openapitools.client.api.PetApi delegate;

	public PetApi(org.openapitools.client.api.PetApi delegate) {
	    this.delegate = delegate;
    }

	public org.openapitools.client.api.PetApi getDelegate() {
	    return delegate;
	}

    /**
     * Add a new pet to the store
     * 
     * @param body Pet object that needs to be added to the store (required)
     * @param resultHandler Asynchronous result handler
     */
    public void addPet(Pet body, Handler<AsyncResult<Void>> resultHandler) {
        delegate.addPet(body, resultHandler);
    }

    /**
     * Add a new pet to the store
     * 
     * @param body Pet object that needs to be added to the store (required)
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxAddPet(Pet body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.addPet(body, fut);
        }));
    }
    /**
     * Deletes a pet
     * 
     * @param petId Pet id to delete (required)
     * @param apiKey  (optional)
     * @param resultHandler Asynchronous result handler
     */
    public void deletePet(Long petId, String apiKey, Handler<AsyncResult<Void>> resultHandler) {
        delegate.deletePet(petId, apiKey, resultHandler);
    }

    /**
     * Deletes a pet
     * 
     * @param petId Pet id to delete (required)
     * @param apiKey  (optional)
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxDeletePet(Long petId, String apiKey) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.deletePet(petId, apiKey, fut);
        }));
    }
    /**
     * Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     * @param status Status values that need to be considered for filter (required)
     * @param resultHandler Asynchronous result handler
     */
    public void findPetsByStatus(List<String> status, Handler<AsyncResult<List<Pet>>> resultHandler) {
        delegate.findPetsByStatus(status, resultHandler);
    }

    /**
     * Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     * @param status Status values that need to be considered for filter (required)
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<List<Pet>> rxFindPetsByStatus(List<String> status) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.findPetsByStatus(status, fut);
        }));
    }
    /**
     * Finds Pets by tags
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * @param tags Tags to filter by (required)
     * @param resultHandler Asynchronous result handler
     */
    public void findPetsByTags(Set<String> tags, Handler<AsyncResult<Set<Pet>>> resultHandler) {
        delegate.findPetsByTags(tags, resultHandler);
    }

    /**
     * Finds Pets by tags
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * @param tags Tags to filter by (required)
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Set<Pet>> rxFindPetsByTags(Set<String> tags) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.findPetsByTags(tags, fut);
        }));
    }
    /**
     * Find pet by ID
     * Returns a single pet
     * @param petId ID of pet to return (required)
     * @param resultHandler Asynchronous result handler
     */
    public void getPetById(Long petId, Handler<AsyncResult<Pet>> resultHandler) {
        delegate.getPetById(petId, resultHandler);
    }

    /**
     * Find pet by ID
     * Returns a single pet
     * @param petId ID of pet to return (required)
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Pet> rxGetPetById(Long petId) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.getPetById(petId, fut);
        }));
    }
    /**
     * Update an existing pet
     * 
     * @param body Pet object that needs to be added to the store (required)
     * @param resultHandler Asynchronous result handler
     */
    public void updatePet(Pet body, Handler<AsyncResult<Void>> resultHandler) {
        delegate.updatePet(body, resultHandler);
    }

    /**
     * Update an existing pet
     * 
     * @param body Pet object that needs to be added to the store (required)
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxUpdatePet(Pet body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.updatePet(body, fut);
        }));
    }
    /**
     * Updates a pet in the store with form data
     * 
     * @param petId ID of pet that needs to be updated (required)
     * @param name Updated name of the pet (optional)
     * @param status Updated status of the pet (optional)
     * @param resultHandler Asynchronous result handler
     */
    public void updatePetWithForm(Long petId, String name, String status, Handler<AsyncResult<Void>> resultHandler) {
        delegate.updatePetWithForm(petId, name, status, resultHandler);
    }

    /**
     * Updates a pet in the store with form data
     * 
     * @param petId ID of pet that needs to be updated (required)
     * @param name Updated name of the pet (optional)
     * @param status Updated status of the pet (optional)
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxUpdatePetWithForm(Long petId, String name, String status) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.updatePetWithForm(petId, name, status, fut);
        }));
    }
    /**
     * uploads an image
     * 
     * @param petId ID of pet to update (required)
     * @param additionalMetadata Additional data to pass to server (optional)
     * @param file file to upload (optional)
     * @param resultHandler Asynchronous result handler
     */
    public void uploadFile(Long petId, String additionalMetadata, AsyncFile file, Handler<AsyncResult<ModelApiResponse>> resultHandler) {
        delegate.uploadFile(petId, additionalMetadata, file, resultHandler);
    }

    /**
     * uploads an image
     * 
     * @param petId ID of pet to update (required)
     * @param additionalMetadata Additional data to pass to server (optional)
     * @param file file to upload (optional)
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<ModelApiResponse> rxUploadFile(Long petId, String additionalMetadata, AsyncFile file) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.uploadFile(petId, additionalMetadata, file, fut);
        }));
    }
    /**
     * uploads an image (required)
     * 
     * @param petId ID of pet to update (required)
     * @param requiredFile file to upload (required)
     * @param additionalMetadata Additional data to pass to server (optional)
     * @param resultHandler Asynchronous result handler
     */
    public void uploadFileWithRequiredFile(Long petId, AsyncFile requiredFile, String additionalMetadata, Handler<AsyncResult<ModelApiResponse>> resultHandler) {
        delegate.uploadFileWithRequiredFile(petId, requiredFile, additionalMetadata, resultHandler);
    }

    /**
     * uploads an image (required)
     * 
     * @param petId ID of pet to update (required)
     * @param requiredFile file to upload (required)
     * @param additionalMetadata Additional data to pass to server (optional)
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<ModelApiResponse> rxUploadFileWithRequiredFile(Long petId, AsyncFile requiredFile, String additionalMetadata) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.uploadFileWithRequiredFile(petId, requiredFile, additionalMetadata, fut);
        }));
    }

    public static PetApi newInstance(org.openapitools.client.api.PetApi arg) {
        return arg != null ? new PetApi(arg) : null;
    }
}
