package org.openapitools.server.api;

import java.util.ArrayList;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import io.helidon.common.http.DataChunk;
import java.io.File;
import io.helidon.webserver.Handler;
import java.util.HashMap;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Map;
import org.openapitools.server.model.ModelApiResponse;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.openapitools.server.model.Pet;
import io.helidon.media.multipart.ReadableBodyPart;
import java.util.Set;
import java.io.UncheckedIOException;

import java.util.Optional;
import java.util.logging.Logger;

import io.helidon.common.GenericType;
import io.helidon.common.reactive.Single;
import io.helidon.config.Config;
import io.helidon.webserver.Routing;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;
import io.helidon.webserver.Service;


public abstract class PetService implements Service { 

    protected static final Logger LOGGER = Logger.getLogger(PetService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();

    protected final Config config;

    public PetService(Config config) {
        this.config = config;
    }

    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    public void update(Routing.Rules rules) {
        rules.post("/pet", Handler.create(Pet.class, this::addPet));
        rules.delete("/pet/{petId}", this::deletePet);
        rules.get("/pet/findByStatus", this::findPetsByStatus);
        rules.get("/pet/findByTags", this::findPetsByTags);
        rules.get("/pet/{petId}", this::getPetById);
        rules.put("/pet", Handler.create(Pet.class, this::updatePet));
        rules.post("/pet/{petId}", this::updatePetWithForm);
        rules.post("/pet/{petId}/uploadImage", this::uploadFile);
        rules.post("/fake/{petId}/uploadImageWithRequiredFile", this::uploadFileWithRequiredFile);
    }


    private void processNonFileFormField(String name, Map<String, List<String>> nonFileFormContent, ReadableBodyPart part) {
        List<String> content = nonFileFormContent.computeIfAbsent(name, key -> new ArrayList<>());
        part.content().as(String.class).thenAccept(content::add);
    }

    private void processFileFormField(String name, Map<String, List<InputStream>> fileFormContent, ReadableBodyPart part) {
        List<InputStream> content = fileFormContent.computeIfAbsent(name, key -> new ArrayList<>());
        part.content().map(DataChunk::bytes)
            .collect(ByteArrayOutputStream::new, (stream, bytes) -> {
                try {
                    stream.write(bytes);
                } catch (IOException e) {
                    throw new UncheckedIOException(e);
                }
        })
        .thenAccept(byteStream -> content.add(new ByteArrayInputStream(byteStream.toByteArray())));
    }


    /**
     * POST /pet : Add a new pet to the store.
     * @param request the server request
     * @param response the server response
     * @param pet Pet object that needs to be added to the store 
     */
    void addPet(ServerRequest request, ServerResponse response, Pet pet) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                ValidatorUtils.checkNonNull(pet);
                
                handleAddPet(request, response, pet);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle POST /pet : Add a new pet to the store.
     * @param request the server request
     * @param response the server response
     * @param pet Pet object that needs to be added to the store 
     */
    abstract void handleAddPet(ServerRequest request, ServerResponse response, Pet pet);


    /**
     * DELETE /pet/{petId} : Deletes a pet.
     * @param request the server request
     * @param response the server response
     */
    void deletePet(ServerRequest request, ServerResponse response) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                Long petId = Optional.ofNullable(request.path().param("petId")).map(Long::valueOf).orElse(null);
                ValidatorUtils.checkNonNull(petId);
                String apiKey = request.headers().value("api_key").orElse(null);
                
                handleDeletePet(request, response, petId, apiKey);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle DELETE /pet/{petId} : Deletes a pet.
     * @param request the server request
     * @param response the server response
     * @param petId Pet id to delete 
     * @param apiKey apiKey 
     */
    abstract void handleDeletePet(ServerRequest request, ServerResponse response, Long petId, String apiKey);


    /**
     * GET /pet/findByStatus : Finds Pets by status.
     * @param request the server request
     * @param response the server response
     */
    void findPetsByStatus(ServerRequest request, ServerResponse response) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                List<String> status = Optional.ofNullable(request.queryParams().toMap().get("status")).orElse(null);
                ValidatorUtils.checkNonNull(status);
                
                handleFindPetsByStatus(request, response, status);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle GET /pet/findByStatus : Finds Pets by status.
     * @param request the server request
     * @param response the server response
     * @param status Status values that need to be considered for filter 
     */
    abstract void handleFindPetsByStatus(ServerRequest request, ServerResponse response, List<String> status);


    /**
     * GET /pet/findByTags : Finds Pets by tags.
     * @param request the server request
     * @param response the server response
     */
    void findPetsByTags(ServerRequest request, ServerResponse response) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                List<String> tags = Optional.ofNullable(request.queryParams().toMap().get("tags")).orElse(null);
                ValidatorUtils.checkNonNull(tags);
                
                handleFindPetsByTags(request, response, tags);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle GET /pet/findByTags : Finds Pets by tags.
     * @param request the server request
     * @param response the server response
     * @param tags Tags to filter by 
     */
    abstract void handleFindPetsByTags(ServerRequest request, ServerResponse response, List<String> tags);


    /**
     * GET /pet/{petId} : Find pet by ID.
     * @param request the server request
     * @param response the server response
     */
    void getPetById(ServerRequest request, ServerResponse response) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                Long petId = Optional.ofNullable(request.path().param("petId")).map(Long::valueOf).orElse(null);
                ValidatorUtils.checkNonNull(petId);
                
                handleGetPetById(request, response, petId);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle GET /pet/{petId} : Find pet by ID.
     * @param request the server request
     * @param response the server response
     * @param petId ID of pet to return 
     */
    abstract void handleGetPetById(ServerRequest request, ServerResponse response, Long petId);


    /**
     * PUT /pet : Update an existing pet.
     * @param request the server request
     * @param response the server response
     * @param pet Pet object that needs to be added to the store 
     */
    void updatePet(ServerRequest request, ServerResponse response, Pet pet) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                ValidatorUtils.checkNonNull(pet);
                
                handleUpdatePet(request, response, pet);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle PUT /pet : Update an existing pet.
     * @param request the server request
     * @param response the server response
     * @param pet Pet object that needs to be added to the store 
     */
    abstract void handleUpdatePet(ServerRequest request, ServerResponse response, Pet pet);


    /**
     * POST /pet/{petId} : Updates a pet in the store with form data.
     * @param request the server request
     * @param response the server response
     */
    void updatePetWithForm(ServerRequest request, ServerResponse response) { 
        Map<String, List<String>> nonFileFormContent = new HashMap<>();
        Map<String, List<InputStream>> fileFormContent = new HashMap<>();
        Single<Void> formSingle = request.content().asStream(ReadableBodyPart.class)
                                         .forEach(part -> {
                                            String name = part.name();
                                            if ("name".equals(name)) {
                                                processNonFileFormField(name, nonFileFormContent, part);
                                            }
                                            if ("status".equals(name)) {
                                                processNonFileFormField(name, nonFileFormContent, part);
                                            }
                                            part.drain();
                                         });
        Single.create(formSingle)
            .thenAccept(val -> {
                Long petId = Optional.ofNullable(request.path().param("petId")).map(Long::valueOf).orElse(null);
                ValidatorUtils.checkNonNull(petId);
                String name = Optional.ofNullable(nonFileFormContent.get("name")).flatMap(list->list.stream().findFirst()).orElse(null);
                String status = Optional.ofNullable(nonFileFormContent.get("status")).flatMap(list->list.stream().findFirst()).orElse(null);
                
                handleUpdatePetWithForm(request, response, petId, name, status);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle POST /pet/{petId} : Updates a pet in the store with form data.
     * @param request the server request
     * @param response the server response
     * @param petId ID of pet that needs to be updated 
     * @param name Updated name of the pet 
     * @param status Updated status of the pet 
     */
    abstract void handleUpdatePetWithForm(ServerRequest request, ServerResponse response, Long petId, String name, String status);


    /**
     * POST /pet/{petId}/uploadImage : uploads an image.
     * @param request the server request
     * @param response the server response
     */
    void uploadFile(ServerRequest request, ServerResponse response) { 
        Map<String, List<String>> nonFileFormContent = new HashMap<>();
        Map<String, List<InputStream>> fileFormContent = new HashMap<>();
        Single<Void> formSingle = request.content().asStream(ReadableBodyPart.class)
                                         .forEach(part -> {
                                            String name = part.name();
                                            if ("additionalMetadata".equals(name)) {
                                                processNonFileFormField(name, nonFileFormContent, part);
                                            }
                                            if ("file".equals(name)) {
                                                processFileFormField(name, fileFormContent, part);
                                            }
                                            part.drain();
                                         });
        Single.create(formSingle)
            .thenAccept(val -> {
                Long petId = Optional.ofNullable(request.path().param("petId")).map(Long::valueOf).orElse(null);
                ValidatorUtils.checkNonNull(petId);
                String additionalMetadata = Optional.ofNullable(nonFileFormContent.get("additionalMetadata")).flatMap(list->list.stream().findFirst()).orElse(null);
                InputStream _file = Optional.ofNullable(fileFormContent.get("file")).flatMap(list->list.stream().findFirst()).orElse(null);
                
                handleUploadFile(request, response, petId, additionalMetadata, _file);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle POST /pet/{petId}/uploadImage : uploads an image.
     * @param request the server request
     * @param response the server response
     * @param petId ID of pet to update 
     * @param additionalMetadata Additional data to pass to server 
     * @param _file file to upload 
     */
    abstract void handleUploadFile(ServerRequest request, ServerResponse response, Long petId, String additionalMetadata, InputStream _file);


    /**
     * POST /fake/{petId}/uploadImageWithRequiredFile : uploads an image (required).
     * @param request the server request
     * @param response the server response
     */
    void uploadFileWithRequiredFile(ServerRequest request, ServerResponse response) { 
        Map<String, List<String>> nonFileFormContent = new HashMap<>();
        Map<String, List<InputStream>> fileFormContent = new HashMap<>();
        Single<Void> formSingle = request.content().asStream(ReadableBodyPart.class)
                                         .forEach(part -> {
                                            String name = part.name();
                                            if ("additionalMetadata".equals(name)) {
                                                processNonFileFormField(name, nonFileFormContent, part);
                                            }
                                            if ("requiredFile".equals(name)) {
                                                processFileFormField(name, fileFormContent, part);
                                            }
                                            part.drain();
                                         });
        Single.create(formSingle)
            .thenAccept(val -> {
                Long petId = Optional.ofNullable(request.path().param("petId")).map(Long::valueOf).orElse(null);
                ValidatorUtils.checkNonNull(petId);
                InputStream requiredFile = Optional.ofNullable(fileFormContent.get("requiredFile")).flatMap(list->list.stream().findFirst()).orElse(null);
                ValidatorUtils.checkNonNull(requiredFile);
                String additionalMetadata = Optional.ofNullable(nonFileFormContent.get("additionalMetadata")).flatMap(list->list.stream().findFirst()).orElse(null);
                
                handleUploadFileWithRequiredFile(request, response, petId, requiredFile, additionalMetadata);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle POST /fake/{petId}/uploadImageWithRequiredFile : uploads an image (required).
     * @param request the server request
     * @param response the server response
     * @param petId ID of pet to update 
     * @param requiredFile file to upload 
     * @param additionalMetadata Additional data to pass to server 
     */
    abstract void handleUploadFileWithRequiredFile(ServerRequest request, ServerResponse response, Long petId, InputStream requiredFile, String additionalMetadata);


    abstract Void handleError(ServerRequest request, ServerResponse response, Throwable throwable);
}
