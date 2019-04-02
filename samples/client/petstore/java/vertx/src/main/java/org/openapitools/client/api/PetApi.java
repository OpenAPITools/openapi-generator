package org.openapitools.client.api;

import java.io.InputStream;
import org.openapitools.client.model.ModelApiResponse;
import org.openapitools.client.model.Pet;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface PetApi {

    void addPet(Pet body, Handler<AsyncResult<Void>> handler);

    void deletePet(Long petId, String apiKey, Handler<AsyncResult<Void>> handler);

    void findPetsByStatus(List<String> status, Handler<AsyncResult<List<Pet>>> handler);

    void findPetsByTags(List<String> tags, Handler<AsyncResult<List<Pet>>> handler);

    void getPetById(Long petId, Handler<AsyncResult<Pet>> handler);

    void updatePet(Pet body, Handler<AsyncResult<Void>> handler);

    void updatePetWithForm(Long petId, String name, String status, Handler<AsyncResult<Void>> handler);

    void uploadFile(Long petId, String additionalMetadata, InputStream file, Handler<AsyncResult<ModelApiResponse>> handler);

    void uploadFileWithRequiredFile(Long petId, InputStream requiredFile, String additionalMetadata, Handler<AsyncResult<ModelApiResponse>> handler);

}
