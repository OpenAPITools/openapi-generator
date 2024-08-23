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
import io.helidon.http.media.multipart.MultiPart;
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

import io.helidon.webserver.http.HttpRules;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import io.helidon.webserver.http.HttpService;

@io.helidon.common.Generated(value = "org.openapitools.codegen.languages.JavaHelidonServerCodegen",
                             trigger = "tag = 'pet'",
                             version = "stable")
public abstract class PetService implements HttpService {


    protected static final ObjectMapper MAPPER = JsonProvider.objectMapper();

    protected AddPetOp addPetOp = createAddPetOp();
    protected DeletePetOp deletePetOp = createDeletePetOp();
    protected FindPetsByStatusOp findPetsByStatusOp = createFindPetsByStatusOp();
    protected FindPetsByTagsOp findPetsByTagsOp = createFindPetsByTagsOp();
    protected GetPetByIdOp getPetByIdOp = createGetPetByIdOp();
    protected UpdatePetOp updatePetOp = createUpdatePetOp();
    protected UpdatePetWithFormOp updatePetWithFormOp = createUpdatePetWithFormOp();
    protected UploadFileOp uploadFileOp = createUploadFileOp();


    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    public void routing(HttpRules rules) {
        rules.post("/", this::addPet);
        rules.delete("/{petId}", this::deletePet);
        rules.get("/findByStatus", this::findPetsByStatus);
        rules.get("/findByTags", this::findPetsByTags);
        rules.get("/{petId}", this::getPetById);
        rules.put("/", this::updatePet);
        rules.post("/{petId}", this::updatePetWithForm);
        rules.post("/{petId}/uploadImage", this::uploadFile);
    }


    /**
     * POST /pet : Add a new pet to the store.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void addPet(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        // Parameter: Pet
        Pet pet = addPetOp.pet(request, validator);
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

        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        // Parameter: petId
        Long petId = deletePetOp.petId(request, validator);

        validator.require("petId", petId);

        // Parameter: api_key
        Optional<String> apiKey = deletePetOp.apiKey(request, validator);

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

        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        // Parameter: status
        List<String> status = findPetsByStatusOp.status(request, validator);

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

        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        // Parameter: tags
        Set<String> tags = findPetsByTagsOp.tags(request, validator);

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

        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        // Parameter: petId
        Long petId = getPetByIdOp.petId(request, validator);

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

        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        // Parameter: Pet
        Pet pet = updatePetOp.pet(request, validator);
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

        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        
        Parameters formParams = request.content().as(Parameters.class);

        // Parameter: petId
        Long petId = updatePetWithFormOp.petId(request, validator);

        validator.require("petId", petId);

        // Parameter: name
        Optional<String> name = updatePetWithFormOp.name(request, formParams, validator);


        // Parameter: status
        Optional<String> status = updatePetWithFormOp.status(request, formParams, validator);

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

        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        
        Map<String, ReadablePart> parts = PartsUtils.partsMap(request);

        // Parameter: petId
        Long petId = uploadFileOp.petId(request, validator);

        validator.require("petId", petId);

        // Parameter: additionalMetadata
        Optional<ReadablePart> additionalMetadata = uploadFileOp.additionalMetadata(request, parts, validator);


        // Parameter: file
        Optional<ReadablePart> _file = uploadFileOp._file(request, parts, validator);

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
     * Returns a new instance of the class which handles parameters to and responses from the addPet operation.
     * <p>
     *     Developers can override this method if they extend the PetService class.
     * </p>
     *
     * @return new AddPet
     */
    protected AddPetOp createAddPetOp() {
        return new AddPetOp();
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
    public static class AddPetOp {

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
         * Response for HTTP status code {@code 200}.
         */
        record Response200() {

            /**
             * Creates a response builder for the status {@code 200} response
             * for the addPet operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response200> {

                @Override
                public Response200 build() {
                    return new Response200();
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.OK_200);
                _serverResponse.send();
            }
        }

        /**
         * Response for HTTP status code {@code 405}.
         */
        record Response405() {

            /**
             * Creates a response builder for the status {@code 405} response
             * for the addPet operation; there are no required result values for this response.
             *
             * @return new builder for status 405
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response405 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response405> {

                @Override
                public Response405 build() {
                    return new Response405();
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.METHOD_NOT_ALLOWED_405);
                _serverResponse.send();
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
    protected DeletePetOp createDeletePetOp() {
        return new DeletePetOp();
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
    public static class DeletePetOp {

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
         * Response for HTTP status code {@code 200}.
         */
        record Response200() {

            /**
             * Creates a response builder for the status {@code 200} response
             * for the deletePet operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response200> {

                @Override
                public Response200 build() {
                    return new Response200();
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.OK_200);
                _serverResponse.send();
            }
        }

        /**
         * Response for HTTP status code {@code 400}.
         */
        record Response400() {

            /**
             * Creates a response builder for the status {@code 400} response
             * for the deletePet operation; there are no required result values for this response.
             *
             * @return new builder for status 400
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response400 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response400> {

                @Override
                public Response400 build() {
                    return new Response400();
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.BAD_REQUEST_400);
                _serverResponse.send();
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
    protected FindPetsByStatusOp createFindPetsByStatusOp() {
        return new FindPetsByStatusOp();
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
    public static class FindPetsByStatusOp {

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
         * Response for HTTP status code {@code 200}.
        *
         * @param response 
         */
        record Response200(List<Pet> response) {

            /**
             * Creates a response builder for the status {@code 200} response
             * for the findPetsByStatus operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response200> {

                private List<Pet> response;
                @Override
                public Response200 build() {
                    return new Response200(response);
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
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
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.OK_200);
                if (response != null) { 
                _serverResponse.send(response);
                } else {
                    _serverResponse.send();
                }
            }
        }

        /**
         * Response for HTTP status code {@code 400}.
         */
        record Response400() {

            /**
             * Creates a response builder for the status {@code 400} response
             * for the findPetsByStatus operation; there are no required result values for this response.
             *
             * @return new builder for status 400
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response400 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response400> {

                @Override
                public Response400 build() {
                    return new Response400();
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.BAD_REQUEST_400);
                _serverResponse.send();
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
    protected FindPetsByTagsOp createFindPetsByTagsOp() {
        return new FindPetsByTagsOp();
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
    public static class FindPetsByTagsOp {

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
         * Response for HTTP status code {@code 200}.
        *
         * @param response 
         */
        record Response200(Set<Pet> response) {

            /**
             * Creates a response builder for the status {@code 200} response
             * for the findPetsByTags operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response200> {

                private Set<Pet> response;
                @Override
                public Response200 build() {
                    return new Response200(response);
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
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
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.OK_200);
                if (response != null) { 
                _serverResponse.send(response);
                } else {
                    _serverResponse.send();
                }
            }
        }

        /**
         * Response for HTTP status code {@code 400}.
         */
        record Response400() {

            /**
             * Creates a response builder for the status {@code 400} response
             * for the findPetsByTags operation; there are no required result values for this response.
             *
             * @return new builder for status 400
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response400 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response400> {

                @Override
                public Response400 build() {
                    return new Response400();
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.BAD_REQUEST_400);
                _serverResponse.send();
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
    protected GetPetByIdOp createGetPetByIdOp() {
        return new GetPetByIdOp();
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
    public static class GetPetByIdOp {

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
         * Response for HTTP status code {@code 200}.
        *
         * @param response 
         */
        record Response200(Pet response) {

            /**
             * Creates a response builder for the status {@code 200} response
             * for the getPetById operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response200> {

                private Pet response;
                @Override
                public Response200 build() {
                    return new Response200(response);
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
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
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.OK_200);
                if (response != null) { 
                _serverResponse.send(response);
                } else {
                    _serverResponse.send();
                }
            }
        }

        /**
         * Response for HTTP status code {@code 400}.
         */
        record Response400() {

            /**
             * Creates a response builder for the status {@code 400} response
             * for the getPetById operation; there are no required result values for this response.
             *
             * @return new builder for status 400
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response400 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response400> {

                @Override
                public Response400 build() {
                    return new Response400();
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.BAD_REQUEST_400);
                _serverResponse.send();
            }
        }

        /**
         * Response for HTTP status code {@code 404}.
         */
        record Response404() {

            /**
             * Creates a response builder for the status {@code 404} response
             * for the getPetById operation; there are no required result values for this response.
             *
             * @return new builder for status 404
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response404 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response404> {

                @Override
                public Response404 build() {
                    return new Response404();
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.NOT_FOUND_404);
                _serverResponse.send();
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
    protected UpdatePetOp createUpdatePetOp() {
        return new UpdatePetOp();
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
    public static class UpdatePetOp {

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
         * Response for HTTP status code {@code 200}.
         */
        record Response200() {

            /**
             * Creates a response builder for the status {@code 200} response
             * for the updatePet operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response200> {

                @Override
                public Response200 build() {
                    return new Response200();
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.OK_200);
                _serverResponse.send();
            }
        }

        /**
         * Response for HTTP status code {@code 400}.
         */
        record Response400() {

            /**
             * Creates a response builder for the status {@code 400} response
             * for the updatePet operation; there are no required result values for this response.
             *
             * @return new builder for status 400
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response400 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response400> {

                @Override
                public Response400 build() {
                    return new Response400();
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.BAD_REQUEST_400);
                _serverResponse.send();
            }
        }

        /**
         * Response for HTTP status code {@code 404}.
         */
        record Response404() {

            /**
             * Creates a response builder for the status {@code 404} response
             * for the updatePet operation; there are no required result values for this response.
             *
             * @return new builder for status 404
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response404 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response404> {

                @Override
                public Response404 build() {
                    return new Response404();
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.NOT_FOUND_404);
                _serverResponse.send();
            }
        }

        /**
         * Response for HTTP status code {@code 405}.
         */
        record Response405() {

            /**
             * Creates a response builder for the status {@code 405} response
             * for the updatePet operation; there are no required result values for this response.
             *
             * @return new builder for status 405
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response405 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response405> {

                @Override
                public Response405 build() {
                    return new Response405();
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.METHOD_NOT_ALLOWED_405);
                _serverResponse.send();
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
    protected UpdatePetWithFormOp createUpdatePetWithFormOp() {
        return new UpdatePetWithFormOp();
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
    public static class UpdatePetWithFormOp {

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
         * Response for HTTP status code {@code 200}.
         */
        record Response200() {

            /**
             * Creates a response builder for the status {@code 200} response
             * for the updatePetWithForm operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response200> {

                @Override
                public Response200 build() {
                    return new Response200();
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.OK_200);
                _serverResponse.send();
            }
        }

        /**
         * Response for HTTP status code {@code 405}.
         */
        record Response405() {

            /**
             * Creates a response builder for the status {@code 405} response
             * for the updatePetWithForm operation; there are no required result values for this response.
             *
             * @return new builder for status 405
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response405 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response405> {

                @Override
                public Response405 build() {
                    return new Response405();
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
                }
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.METHOD_NOT_ALLOWED_405);
                _serverResponse.send();
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
    protected UploadFileOp createUploadFileOp() {
        return new UploadFileOp();
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
    public static class UploadFileOp {

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
         * Response for HTTP status code {@code 200}.
        *
         * @param response 
         */
        record Response200(ModelApiResponse response) {

            /**
             * Creates a response builder for the status {@code 200} response
             * for the uploadFile operation; there are no required result values for this response.
             *
             * @return new builder for status 200
             */
            static Builder builder() {
                return new Builder();
            }

            /**
             * Builder for the Response200 result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Response200> {

                private ModelApiResponse response;
                @Override
                public Response200 build() {
                    return new Response200(response);
                }

                /**
                 * Sends the response data in this builder to the specified {@link io.helidon.webserver.http.ServerResponse},
                 * assigning the HTTP status, any response headers, and any response entity.
                 * <p>
                 *     Equivalent to {@snippet :
                 *     build().send(_serverResponse);
                 *     }
                 * </p>
                 *
                 * @param _serverResponse the {@code ServerResponse} to which to apply the status, headers, and entity
                 */
                void send(ServerResponse _serverResponse) {
                    build().send(_serverResponse);
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
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.OK_200);
                if (response != null) { 
                _serverResponse.send(response);
                } else {
                    _serverResponse.send();
                }
            }
        }
    }


    @Override
    public void afterStop() {
    System.out.println("Service PetService is down. Goodbye!");
    }


}
