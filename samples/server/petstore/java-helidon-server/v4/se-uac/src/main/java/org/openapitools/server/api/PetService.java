package org.openapitools.server.api;

import java.util.ArrayList;
import java.util.stream.Collectors;
import java.io.File;
import java.util.HashMap;
import io.helidon.http.HeaderNames;
import io.helidon.http.Headers;
import java.util.HexFormat;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Map;
import org.openapitools.server.model.ModelApiResponse;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Optional;
import io.helidon.common.parameters.Parameters;
import org.openapitools.server.model.Pet;
import io.helidon.http.media.multipart.ReadablePart;
import java.util.Set;
import io.helidon.http.Status;
import java.io.UncheckedIOException;
import io.helidon.common.mapper.Value;

import java.util.Optional;
import java.util.logging.Logger;

import io.helidon.webserver.http.HttpRules;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import io.helidon.webserver.http.HttpService;

@io.helidon.common.Generated(value = "org.openapitools.codegen.languages.JavaHelidonServerCodegen",
                             trigger = "tag = 'Pet'",
                             version = "stable")
public abstract class PetService implements HttpService {

    protected static final Logger LOGGER = Logger.getLogger(PetService.class.getName());
    protected static final ObjectMapper MAPPER = JsonProvider.objectMapper();

    protected AddPet addPet = addPet();
    protected DeletePet deletePet = deletePet();
    protected FindPetsByStatus findPetsByStatus = findPetsByStatus();
    protected FindPetsByTags findPetsByTags = findPetsByTags();
    protected GetPetById getPetById = getPetById();
    protected UpdatePet updatePet = updatePet();
    protected UpdatePetWithForm updatePetWithForm = updatePetWithForm();
    protected UploadFile uploadFile = uploadFile();
    protected UploadFileWithRequiredFile uploadFileWithRequiredFile = uploadFileWithRequiredFile();


    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    public void routing(HttpRules rules) {
        rules.post("/pet", this::addPet);
        rules.delete("/pet/{petId}", this::deletePet);
        rules.get("/pet/findByStatus", this::findPetsByStatus);
        rules.get("/pet/findByTags", this::findPetsByTags);
        rules.get("/pet/{petId}", this::getPetById);
        rules.put("/pet", this::updatePet);
        rules.post("/pet/{petId}", this::updatePetWithForm);
        rules.post("/pet/{petId}/uploadImage", this::uploadFile);
        rules.post("/fake/{petId}/uploadImageWithRequiredFile", this::uploadFileWithRequiredFile);
    }


    /**
     * POST /pet : Add a new pet to the store.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void addPet(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: Pet
        Pet pet = addPet.pet(request, validator);
        validator.require("pet", pet);

        validator.execute();

        handleAddPet(request, response, 
                    pet);
    }

    /**
     * Handle POST /pet : Add a new pet to the store.
     *
     * @param request the server request
     * @param response the server response
     * @param pet Pet object that needs to be added to the store 
     */
    protected abstract void handleAddPet(ServerRequest request, ServerResponse response, 
                Pet pet);

    /**
     * DELETE /pet/{petId} : Deletes a pet.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void deletePet(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: petId
        Long petId = deletePet.petId(request, validator);

        validator.require("petId", petId);

        // Parameter: api_key
        Optional<String> apiKey = deletePet.apiKey(request, validator);

        validator.execute();

        handleDeletePet(request, response, 
                    petId, 
                    apiKey);
    }

    /**
     * Handle DELETE /pet/{petId} : Deletes a pet.
     *
     * @param request the server request
     * @param response the server response
     * @param petId Pet id to delete 
     * @param apiKey apiKey 
     */
    protected abstract void handleDeletePet(ServerRequest request, ServerResponse response, 
                Long petId, 
                Optional<String> apiKey);

    /**
     * GET /pet/findByStatus : Finds Pets by status.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void findPetsByStatus(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: status
        List<String> status = findPetsByStatus.status(request, validator);

        validator.require("status", status);
        validator.execute();

        handleFindPetsByStatus(request, response, 
                    status);
    }

    /**
     * Handle GET /pet/findByStatus : Finds Pets by status.
     *
     * @param request the server request
     * @param response the server response
     * @param status Status values that need to be considered for filter 
     */
    protected abstract void handleFindPetsByStatus(ServerRequest request, ServerResponse response, 
                List<String> status);

    /**
     * GET /pet/findByTags : Finds Pets by tags.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void findPetsByTags(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: tags
        Set<String> tags = findPetsByTags.tags(request, validator);

        validator.require("tags", tags);
        validator.execute();

        handleFindPetsByTags(request, response, 
                    tags);
    }

    /**
     * Handle GET /pet/findByTags : Finds Pets by tags.
     *
     * @param request the server request
     * @param response the server response
     * @param tags Tags to filter by 
     */
    protected abstract void handleFindPetsByTags(ServerRequest request, ServerResponse response, 
                Set<String> tags);

    /**
     * GET /pet/{petId} : Find pet by ID.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void getPetById(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: petId
        Long petId = getPetById.petId(request, validator);

        validator.require("petId", petId);
        validator.execute();

        handleGetPetById(request, response, 
                    petId);
    }

    /**
     * Handle GET /pet/{petId} : Find pet by ID.
     *
     * @param request the server request
     * @param response the server response
     * @param petId ID of pet to return 
     */
    protected abstract void handleGetPetById(ServerRequest request, ServerResponse response, 
                Long petId);

    /**
     * PUT /pet : Update an existing pet.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void updatePet(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: Pet
        Pet pet = updatePet.pet(request, validator);
        validator.require("pet", pet);

        validator.execute();

        handleUpdatePet(request, response, 
                    pet);
    }

    /**
     * Handle PUT /pet : Update an existing pet.
     *
     * @param request the server request
     * @param response the server response
     * @param pet Pet object that needs to be added to the store 
     */
    protected abstract void handleUpdatePet(ServerRequest request, ServerResponse response, 
                Pet pet);

    /**
     * POST /pet/{petId} : Updates a pet in the store with form data.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void updatePetWithForm(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        
        Parameters formParams = request.content().as(Parameters.class);

        // Parameter: petId
        Long petId = updatePetWithForm.petId(request, validator);

        validator.require("petId", petId);

        // Parameter: name
        Optional<String> name = updatePetWithForm.name(request, formParams, validator);


        // Parameter: status
        Optional<String> status = updatePetWithForm.status(request, formParams, validator);

        validator.execute();

        handleUpdatePetWithForm(request, response, 
                    petId, 
                    name, 
                    status);
    }

    /**
     * Handle POST /pet/{petId} : Updates a pet in the store with form data.
     *
     * @param request the server request
     * @param response the server response
     * @param petId ID of pet that needs to be updated 
     * @param name Updated name of the pet 
     * @param status Updated status of the pet 
     */
    protected abstract void handleUpdatePetWithForm(ServerRequest request, ServerResponse response, 
                Long petId, 
                Optional<String> name, 
                Optional<String> status);

    /**
     * POST /pet/{petId}/uploadImage : uploads an image.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void uploadFile(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        
        Map<String, ReadablePart> parts = PartsUtils.partsMap(request);

        // Parameter: petId
        Long petId = uploadFile.petId(request, validator);

        validator.require("petId", petId);

        // Parameter: additionalMetadata
        Optional<ReadablePart> additionalMetadata = uploadFile.additionalMetadata(request, parts, validator);


        // Parameter: file
        Optional<ReadablePart> _file = uploadFile._file(request, parts, validator);

        validator.execute();

        handleUploadFile(request, response, 
                    petId, 
                    additionalMetadata, 
                    _file);
    }

    /**
     * Handle POST /pet/{petId}/uploadImage : uploads an image.
     *
     * @param request the server request
     * @param response the server response
     * @param petId ID of pet to update 
     * @param additionalMetadata Additional data to pass to server 
     * @param _file file to upload 
     */
    protected abstract void handleUploadFile(ServerRequest request, ServerResponse response, 
                Long petId, 
                Optional<ReadablePart> additionalMetadata, 
                Optional<ReadablePart> _file);

    /**
     * POST /fake/{petId}/uploadImageWithRequiredFile : uploads an image (required).
     *
     * @param request the server request
     * @param response the server response
     */
    protected void uploadFileWithRequiredFile(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        
        Map<String, ReadablePart> parts = PartsUtils.partsMap(request);

        // Parameter: petId
        Long petId = uploadFileWithRequiredFile.petId(request, validator);

        validator.require("petId", petId);

        // Parameter: requiredFile
        ReadablePart requiredFile = uploadFileWithRequiredFile.requiredFile(request, parts, validator);

        validator.require("requiredFile", requiredFile);

        // Parameter: additionalMetadata
        Optional<ReadablePart> additionalMetadata = uploadFileWithRequiredFile.additionalMetadata(request, parts, validator);

        validator.execute();

        handleUploadFileWithRequiredFile(request, response, 
                    petId, 
                    requiredFile, 
                    additionalMetadata);
    }

    /**
     * Handle POST /fake/{petId}/uploadImageWithRequiredFile : uploads an image (required).
     *
     * @param request the server request
     * @param response the server response
     * @param petId ID of pet to update 
     * @param requiredFile file to upload 
     * @param additionalMetadata Additional data to pass to server 
     */
    protected abstract void handleUploadFileWithRequiredFile(ServerRequest request, ServerResponse response, 
                Long petId, 
                ReadablePart requiredFile, 
                Optional<ReadablePart> additionalMetadata);

    /**
     * Returns a new instance of the class which handles parameters to and responses from the addPet operation.
     * <p>
     *     Developers can override this method if they extend the PetService class.
     * </p>
     *
     * @return new AddPet
     */
    protected AddPet addPet() {
        return new AddPet();
    }

    /**
     * Helper elements for the {@code addPet} operation.
     * <p>
     * Also below are records for each response declared in the OpenAPI document, organized by response status.
     * <p>
     *     Once your code determines which (if any) declared response to send it can use the static {@code builder} method for
     *     that specific result, passing the required elements of the response as parameters, and then assign any optional
     *     response elements using the other builder methods.
     * <p>
     *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
     *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
     *     the response including any appropriate entity.
     * </p>
     */
    public static class AddPet {

        /**
         * Prepares the pet parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return pet parameter value
         */
        protected Pet pet(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? request.content().as(Pet.class)
                : null;
        }

        /**
         * Result for HTTP status code {@code 200}.
         */
        record result200() {

            /**
             * Creates a result builder for the status {@code 200} result
             * for the addPet operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result200> {


                @Override
                public result200 build() {
                    return new result200();
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                serverResponse.send();
                return serverResponse;
            }
        }

        /**
         * Result for HTTP status code {@code 405}.
         */
        record result405() {

            /**
             * Creates a result builder for the status {@code 405} result
             * for the addPet operation; there are no required result values for this response.
             *
             * @return new builder for status 405
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result405 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result405> {


                @Override
                public result405 build() {
                    return new result405();
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(405));
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the deletePet operation.
     * <p>
     *     Developers can override this method if they extend the PetService class.
     * </p>
     *
     * @return new DeletePet
     */
    protected DeletePet deletePet() {
        return new DeletePet();
    }

    /**
     * Helper elements for the {@code deletePet} operation.
     * <p>
     * Also below are records for each response declared in the OpenAPI document, organized by response status.
     * <p>
     *     Once your code determines which (if any) declared response to send it can use the static {@code builder} method for
     *     that specific result, passing the required elements of the response as parameters, and then assign any optional
     *     response elements using the other builder methods.
     * <p>
     *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
     *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
     *     the response including any appropriate entity.
     * </p>
     */
    public static class DeletePet {

        /**
         * Prepares the petId parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return petId parameter value
         */
        protected Long petId(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.path()
                .pathParameters()
                .first("petId")
                .asOptional()
                .map(Long::valueOf)
                .orElse(null);
        }

        /**
         * Prepares the apiKey parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return apiKey parameter value
         */
        protected Optional<String> apiKey(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.headers()
                .first(HeaderNames.create("api_key"));
        }

        /**
         * Result for HTTP status code {@code 200}.
         */
        record result200() {

            /**
             * Creates a result builder for the status {@code 200} result
             * for the deletePet operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result200> {


                @Override
                public result200 build() {
                    return new result200();
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                serverResponse.send();
                return serverResponse;
            }
        }

        /**
         * Result for HTTP status code {@code 400}.
         */
        record result400() {

            /**
             * Creates a result builder for the status {@code 400} result
             * for the deletePet operation; there are no required result values for this response.
             *
             * @return new builder for status 400
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result400 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result400> {


                @Override
                public result400 build() {
                    return new result400();
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(400));
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the findPetsByStatus operation.
     * <p>
     *     Developers can override this method if they extend the PetService class.
     * </p>
     *
     * @return new FindPetsByStatus
     */
    protected FindPetsByStatus findPetsByStatus() {
        return new FindPetsByStatus();
    }

    /**
     * Helper elements for the {@code findPetsByStatus} operation.
     * <p>
     * Also below are records for each response declared in the OpenAPI document, organized by response status.
     * <p>
     *     Once your code determines which (if any) declared response to send it can use the static {@code builder} method for
     *     that specific result, passing the required elements of the response as parameters, and then assign any optional
     *     response elements using the other builder methods.
     * <p>
     *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
     *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
     *     the response including any appropriate entity.
     * </p>
     */
    public static class FindPetsByStatus {

        /**
         * Prepares the status parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return status parameter value
         */
        protected List<String> status(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .all("status")
                .stream()
                .map(String::valueOf)
                .map(v -> validator.check("status",
                     v,
                     List.of("available",
                             "pending",
                             "sold")))
                .collect(Collectors.toList());
        }

        /**
         * Result for HTTP status code {@code 200}.
        *
         * @param response 
         */
        record result200(List<Pet> response) {

            /**
             * Creates a result builder for the status {@code 200} result
             * for the findPetsByStatus operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result200> {

                private List<Pet> response;

                @Override
                public result200 build() {
                    return new result200(response);
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }

                /**
                 * Sets the value for the optional return property {@code response}.
                 * @param response 
                 * @return updated result builder
                 */
                Builder response(List<Pet> response) {
                    this.response = response;
                    return this;
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                if (response != null) { 
                serverResponse.send(response);
                } else {
                    serverResponse.send();
                }
                return serverResponse;
            }
        }

        /**
         * Result for HTTP status code {@code 400}.
         */
        record result400() {

            /**
             * Creates a result builder for the status {@code 400} result
             * for the findPetsByStatus operation; there are no required result values for this response.
             *
             * @return new builder for status 400
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result400 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result400> {


                @Override
                public result400 build() {
                    return new result400();
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(400));
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the findPetsByTags operation.
     * <p>
     *     Developers can override this method if they extend the PetService class.
     * </p>
     *
     * @return new FindPetsByTags
     */
    protected FindPetsByTags findPetsByTags() {
        return new FindPetsByTags();
    }

    /**
     * Helper elements for the {@code findPetsByTags} operation.
     * <p>
     * Also below are records for each response declared in the OpenAPI document, organized by response status.
     * <p>
     *     Once your code determines which (if any) declared response to send it can use the static {@code builder} method for
     *     that specific result, passing the required elements of the response as parameters, and then assign any optional
     *     response elements using the other builder methods.
     * <p>
     *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
     *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
     *     the response including any appropriate entity.
     * </p>
     */
    public static class FindPetsByTags {

        /**
         * Prepares the tags parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return tags parameter value
         */
        protected Set<String> tags(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .all("tags")
                .stream()
                .map(String::valueOf)
                .collect(Collectors.toSet());
        }

        /**
         * Result for HTTP status code {@code 200}.
        *
         * @param response 
         */
        record result200(Set<Pet> response) {

            /**
             * Creates a result builder for the status {@code 200} result
             * for the findPetsByTags operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result200> {

                private Set<Pet> response;

                @Override
                public result200 build() {
                    return new result200(response);
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }

                /**
                 * Sets the value for the optional return property {@code response}.
                 * @param response 
                 * @return updated result builder
                 */
                Builder response(Set<Pet> response) {
                    this.response = response;
                    return this;
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                if (response != null) { 
                serverResponse.send(response);
                } else {
                    serverResponse.send();
                }
                return serverResponse;
            }
        }

        /**
         * Result for HTTP status code {@code 400}.
         */
        record result400() {

            /**
             * Creates a result builder for the status {@code 400} result
             * for the findPetsByTags operation; there are no required result values for this response.
             *
             * @return new builder for status 400
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result400 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result400> {


                @Override
                public result400 build() {
                    return new result400();
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(400));
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the getPetById operation.
     * <p>
     *     Developers can override this method if they extend the PetService class.
     * </p>
     *
     * @return new GetPetById
     */
    protected GetPetById getPetById() {
        return new GetPetById();
    }

    /**
     * Helper elements for the {@code getPetById} operation.
     * <p>
     * Also below are records for each response declared in the OpenAPI document, organized by response status.
     * <p>
     *     Once your code determines which (if any) declared response to send it can use the static {@code builder} method for
     *     that specific result, passing the required elements of the response as parameters, and then assign any optional
     *     response elements using the other builder methods.
     * <p>
     *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
     *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
     *     the response including any appropriate entity.
     * </p>
     */
    public static class GetPetById {

        /**
         * Prepares the petId parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return petId parameter value
         */
        protected Long petId(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.path()
                .pathParameters()
                .first("petId")
                .asOptional()
                .map(Long::valueOf)
                .orElse(null);
        }

        /**
         * Result for HTTP status code {@code 200}.
        *
         * @param response 
         */
        record result200(Pet response) {

            /**
             * Creates a result builder for the status {@code 200} result
             * for the getPetById operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result200> {

                private Pet response;

                @Override
                public result200 build() {
                    return new result200(response);
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }

                /**
                 * Sets the value for the optional return property {@code response}.
                 * @param response 
                 * @return updated result builder
                 */
                Builder response(Pet response) {
                    this.response = response;
                    return this;
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                if (response != null) { 
                serverResponse.send(response);
                } else {
                    serverResponse.send();
                }
                return serverResponse;
            }
        }

        /**
         * Result for HTTP status code {@code 400}.
         */
        record result400() {

            /**
             * Creates a result builder for the status {@code 400} result
             * for the getPetById operation; there are no required result values for this response.
             *
             * @return new builder for status 400
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result400 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result400> {


                @Override
                public result400 build() {
                    return new result400();
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(400));
                serverResponse.send();
                return serverResponse;
            }
        }

        /**
         * Result for HTTP status code {@code 404}.
         */
        record result404() {

            /**
             * Creates a result builder for the status {@code 404} result
             * for the getPetById operation; there are no required result values for this response.
             *
             * @return new builder for status 404
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result404 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result404> {


                @Override
                public result404 build() {
                    return new result404();
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(404));
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the updatePet operation.
     * <p>
     *     Developers can override this method if they extend the PetService class.
     * </p>
     *
     * @return new UpdatePet
     */
    protected UpdatePet updatePet() {
        return new UpdatePet();
    }

    /**
     * Helper elements for the {@code updatePet} operation.
     * <p>
     * Also below are records for each response declared in the OpenAPI document, organized by response status.
     * <p>
     *     Once your code determines which (if any) declared response to send it can use the static {@code builder} method for
     *     that specific result, passing the required elements of the response as parameters, and then assign any optional
     *     response elements using the other builder methods.
     * <p>
     *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
     *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
     *     the response including any appropriate entity.
     * </p>
     */
    public static class UpdatePet {

        /**
         * Prepares the pet parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return pet parameter value
         */
        protected Pet pet(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? request.content().as(Pet.class)
                : null;
        }

        /**
         * Result for HTTP status code {@code 200}.
         */
        record result200() {

            /**
             * Creates a result builder for the status {@code 200} result
             * for the updatePet operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result200> {


                @Override
                public result200 build() {
                    return new result200();
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                serverResponse.send();
                return serverResponse;
            }
        }

        /**
         * Result for HTTP status code {@code 400}.
         */
        record result400() {

            /**
             * Creates a result builder for the status {@code 400} result
             * for the updatePet operation; there are no required result values for this response.
             *
             * @return new builder for status 400
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result400 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result400> {


                @Override
                public result400 build() {
                    return new result400();
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(400));
                serverResponse.send();
                return serverResponse;
            }
        }

        /**
         * Result for HTTP status code {@code 404}.
         */
        record result404() {

            /**
             * Creates a result builder for the status {@code 404} result
             * for the updatePet operation; there are no required result values for this response.
             *
             * @return new builder for status 404
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result404 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result404> {


                @Override
                public result404 build() {
                    return new result404();
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(404));
                serverResponse.send();
                return serverResponse;
            }
        }

        /**
         * Result for HTTP status code {@code 405}.
         */
        record result405() {

            /**
             * Creates a result builder for the status {@code 405} result
             * for the updatePet operation; there are no required result values for this response.
             *
             * @return new builder for status 405
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result405 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result405> {


                @Override
                public result405 build() {
                    return new result405();
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(405));
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the updatePetWithForm operation.
     * <p>
     *     Developers can override this method if they extend the PetService class.
     * </p>
     *
     * @return new UpdatePetWithForm
     */
    protected UpdatePetWithForm updatePetWithForm() {
        return new UpdatePetWithForm();
    }

    /**
     * Helper elements for the {@code updatePetWithForm} operation.
     * <p>
     * Also below are records for each response declared in the OpenAPI document, organized by response status.
     * <p>
     *     Once your code determines which (if any) declared response to send it can use the static {@code builder} method for
     *     that specific result, passing the required elements of the response as parameters, and then assign any optional
     *     response elements using the other builder methods.
     * <p>
     *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
     *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
     *     the response including any appropriate entity.
     * </p>
     */
    public static class UpdatePetWithForm {

        /**
         * Prepares the petId parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return petId parameter value
         */
        protected Long petId(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.path()
                .pathParameters()
                .first("petId")
                .asOptional()
                .map(Long::valueOf)
                .orElse(null);
        }

        /**
         * Prepares the name parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return name parameter value
         */
        protected Optional<String> name(ServerRequest request, Parameters formParams, ValidatorUtils.Validator validator) {
            return formParams
                .first("name")
                .asOptional();
        }

        /**
         * Prepares the status parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return status parameter value
         */
        protected Optional<String> status(ServerRequest request, Parameters formParams, ValidatorUtils.Validator validator) {
            return formParams
                .first("status")
                .asOptional();
        }

        /**
         * Result for HTTP status code {@code 200}.
         */
        record result200() {

            /**
             * Creates a result builder for the status {@code 200} result
             * for the updatePetWithForm operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result200> {


                @Override
                public result200 build() {
                    return new result200();
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                serverResponse.send();
                return serverResponse;
            }
        }

        /**
         * Result for HTTP status code {@code 405}.
         */
        record result405() {

            /**
             * Creates a result builder for the status {@code 405} result
             * for the updatePetWithForm operation; there are no required result values for this response.
             *
             * @return new builder for status 405
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result405 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result405> {


                @Override
                public result405 build() {
                    return new result405();
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(405));
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the uploadFile operation.
     * <p>
     *     Developers can override this method if they extend the PetService class.
     * </p>
     *
     * @return new UploadFile
     */
    protected UploadFile uploadFile() {
        return new UploadFile();
    }

    /**
     * Helper elements for the {@code uploadFile} operation.
     * <p>
     * Also below are records for each response declared in the OpenAPI document, organized by response status.
     * <p>
     *     Once your code determines which (if any) declared response to send it can use the static {@code builder} method for
     *     that specific result, passing the required elements of the response as parameters, and then assign any optional
     *     response elements using the other builder methods.
     * <p>
     *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
     *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
     *     the response including any appropriate entity.
     * </p>
     */
    public static class UploadFile {

        /**
         * Prepares the petId parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return petId parameter value
         */
        protected Long petId(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.path()
                .pathParameters()
                .first("petId")
                .asOptional()
                .map(Long::valueOf)
                .orElse(null);
        }

        /**
         * Prepares the additionalMetadata parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param parts {@code Map} of part names to {@link io.helidon.http.media.multipart.ReadablePart} for each part
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return additionalMetadata parameter value
         */
        protected Optional<ReadablePart> additionalMetadata(ServerRequest request, Map<String, ReadablePart> parts, ValidatorUtils.Validator validator) {
            return Optional.ofNullable(parts.get("additionalMetadata"));
        }

        /**
         * Prepares the _file parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param parts {@code Map} of part names to {@link io.helidon.http.media.multipart.ReadablePart} for each part
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return _file parameter value
         */
        protected Optional<ReadablePart> _file(ServerRequest request, Map<String, ReadablePart> parts, ValidatorUtils.Validator validator) {
            return Optional.ofNullable(parts.get("file"));
        }

        /**
         * Result for HTTP status code {@code 200}.
        *
         * @param response 
         */
        record result200(ModelApiResponse response) {

            /**
             * Creates a result builder for the status {@code 200} result
             * for the uploadFile operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result200> {

                private ModelApiResponse response;

                @Override
                public result200 build() {
                    return new result200(response);
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }

                /**
                 * Sets the value for the optional return property {@code response}.
                 * @param response 
                 * @return updated result builder
                 */
                Builder response(ModelApiResponse response) {
                    this.response = response;
                    return this;
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                if (response != null) { 
                serverResponse.send(response);
                } else {
                    serverResponse.send();
                }
                return serverResponse;
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the uploadFileWithRequiredFile operation.
     * <p>
     *     Developers can override this method if they extend the PetService class.
     * </p>
     *
     * @return new UploadFileWithRequiredFile
     */
    protected UploadFileWithRequiredFile uploadFileWithRequiredFile() {
        return new UploadFileWithRequiredFile();
    }

    /**
     * Helper elements for the {@code uploadFileWithRequiredFile} operation.
     * <p>
     * Also below are records for each response declared in the OpenAPI document, organized by response status.
     * <p>
     *     Once your code determines which (if any) declared response to send it can use the static {@code builder} method for
     *     that specific result, passing the required elements of the response as parameters, and then assign any optional
     *     response elements using the other builder methods.
     * <p>
     *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
     *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
     *     the response including any appropriate entity.
     * </p>
     */
    public static class UploadFileWithRequiredFile {

        /**
         * Prepares the petId parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return petId parameter value
         */
        protected Long petId(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.path()
                .pathParameters()
                .first("petId")
                .asOptional()
                .map(Long::valueOf)
                .orElse(null);
        }

        /**
         * Prepares the requiredFile parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param parts {@code Map} of part names to {@link io.helidon.http.media.multipart.ReadablePart} for each part
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return requiredFile parameter value
         */
        protected ReadablePart requiredFile(ServerRequest request, Map<String, ReadablePart> parts, ValidatorUtils.Validator validator) {
            return parts.get("requiredFile");
        }

        /**
         * Prepares the additionalMetadata parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param parts {@code Map} of part names to {@link io.helidon.http.media.multipart.ReadablePart} for each part
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return additionalMetadata parameter value
         */
        protected Optional<ReadablePart> additionalMetadata(ServerRequest request, Map<String, ReadablePart> parts, ValidatorUtils.Validator validator) {
            return Optional.ofNullable(parts.get("additionalMetadata"));
        }

        /**
         * Result for HTTP status code {@code 200}.
        *
         * @param response 
         */
        record result200(ModelApiResponse response) {

            /**
             * Creates a result builder for the status {@code 200} result
             * for the uploadFileWithRequiredFile operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Sets the declared HTTP status and sends the response.
             *
             */
            static void send(ServerResponse serverResponse) {
                builder().apply(serverResponse);
            }

            /**
             * Builder for the result200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, result200> {

                private ModelApiResponse response;

                @Override
                public result200 build() {
                    return new result200(response);
                }

                /**
                 * Applies the result data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().apply(serverResponse);
                 *     }
                 * </p>
                 *
                 * @param serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void apply(ServerResponse serverResponse) {
                    build().apply(serverResponse);
                }

                /**
                 * Sets the value for the optional return property {@code response}.
                 * @param response 
                 * @return updated result builder
                 */
                Builder response(ModelApiResponse response) {
                    this.response = response;
                    return this;
                }
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                if (response != null) { 
                serverResponse.send(response);
                } else {
                    serverResponse.send();
                }
                return serverResponse;
            }
        }
    }


    @Override
    public void afterStop() {
    System.out.println("Service PetService is down. Goodbye!");
    }


}
