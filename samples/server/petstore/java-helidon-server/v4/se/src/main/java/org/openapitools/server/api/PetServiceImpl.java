package org.openapitools.server.api;

import java.io.File;
import io.helidon.http.HeaderNames;
import io.helidon.http.Headers;
import java.util.List;
import org.openapitools.server.model.ModelApiResponse;
import io.helidon.http.media.multipart.MultiPart;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Optional;
import io.helidon.common.parameters.Parameters;
import org.openapitools.server.model.Pet;
import java.util.Set;
import io.helidon.http.Status;
import io.helidon.common.mapper.Value;
import 
import io.helidon.common.Errors;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;
import jakarta.validation.ValidationException;
import org.openapitools.server.model.GenericTypes;

public class PetServiceImpl implements PetService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final System.Logger LOGGER = System.getLogger(PetService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    public void addPet(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        Pet pet = request.content().as(Pet.class);

        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void deletePet(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        Long petId = request.path()
                .pathParameters()
                .first("petId")
                .map(v -> validator.require("petId", v))
                .map(Long::valueOf)
                .orElse(null);
        Optional<String> apiKey = request.headers()
                .first(HeaderNames.create("api_key"))
                .or(Optional::empty);
        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void findPetsByStatus(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        List<String> status = request.query()
                .all("status")
                .stream()
                .map(String::valueOf)
                .map(v -> validator.check("status",
                     v,
                     List.of("available",
                             "pending",
                             "sold")))
                .collect(HCollectors.toRequiredList("status",
                                                    validator);
        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void findPetsByTags(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        Set<String> tags = request.query()
                .all("tags")
                .stream()
                .map(String::valueOf)
                .collect(HCollectors.toRequiredList("tags",
                                                    validator);
        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void getPetById(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        Long petId = request.path()
                .pathParameters()
                .first("petId")
                .map(v -> validator.require("petId", v))
                .map(Long::valueOf)
                .orElse(null);
        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void updatePet(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        Pet pet = request.content().as(Pet.class);

        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void updatePetWithForm(ServerRequest request, ServerResponse response) {
        Parameters formParams = request.content().as(Parameters.class);

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        Long petId = request.path()
                .pathParameters()
                .first("petId")
                .map(v -> validator.require("petId", v))
                .map(Long::valueOf)
                .orElse(null);
        Optional<String> name = formParams
                .first("name")
                .asOptional()
                .or(Optional::empty);
        Optional<String> status = formParams
                .first("status")
                .asOptional()
                .or(Optional::empty);
        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void uploadFile(ServerRequest request, ServerResponse response) {
        MultiPart multiPart = request.content().as(MultiPart.class);
        multiPart.forEachRemaining(part -> {
                // TODO: Insert user-implemented handling of multipart data here.
        });
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        Long petId = request.path()
                .pathParameters()
                .first("petId")
                .map(v -> validator.require("petId", v))
                .map(Long::valueOf)
                .orElse(null);
        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void uploadFileWithRequiredFile(ServerRequest request, ServerResponse response) {
        MultiPart multiPart = request.content().as(MultiPart.class);
        multiPart.forEachRemaining(part -> {
                // TODO: Insert user-implemented handling of multipart data here.
        });
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        Long petId = request.path()
                .pathParameters()
                .first("petId")
                .map(v -> validator.require("petId", v))
                .map(Long::valueOf)
                .orElse(null);
        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void afterStop() {
        System.out.println("Service PetService is down. Goodbye!");
    }




    /**
     * Responses for operation {@code addPet } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface AddPetResult {

        /**
         * Result for HTTP status 200.
         */
        record $200() {

            /**
             * Factory method creating a result for the status 200 result
             * for the addPet operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
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
         * Result for HTTP status 405.
         */
        record $405() {

            /**
             * Factory method creating a result for the status 405 result
             * for the addPet operation, accepting all the required result values.
             *
             * @return new result data for status 405
             */
            static $405 create() {
                return new $405();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
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
     * Responses for operation {@code deletePet } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface DeletePetResult {

        /**
         * Result for HTTP status 200.
         */
        record $200() {

            /**
             * Factory method creating a result for the status 200 result
             * for the deletePet operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
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
         * Result for HTTP status 400.
         */
        record $400() {

            /**
             * Factory method creating a result for the status 400 result
             * for the deletePet operation, accepting all the required result values.
             *
             * @return new result data for status 400
             */
            static $400 create() {
                return new $400();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
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
     * Responses for operation {@code findPetsByStatus } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface FindPetsByStatusResult {

        /**
         * Result for HTTP status 200.
         *
         * @param response 
         */
        record $200(List<Pet> response) {

            /**
             * Factory method creating a result for the status 200 result
             * for the findPetsByStatus operation, accepting all the required result values.
             *
             * @param response returned entity
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200(List.of());
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
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
         * Result for HTTP status 400.
         */
        record $400() {

            /**
             * Factory method creating a result for the status 400 result
             * for the findPetsByStatus operation, accepting all the required result values.
             *
             * @return new result data for status 400
             */
            static $400 create() {
                return new $400();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
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
     * Responses for operation {@code findPetsByTags } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface FindPetsByTagsResult {

        /**
         * Result for HTTP status 200.
         *
         * @param response 
         */
        record $200(Set<Pet> response) {

            /**
             * Factory method creating a result for the status 200 result
             * for the findPetsByTags operation, accepting all the required result values.
             *
             * @param response returned entity
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200(Set.of());
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
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
         * Result for HTTP status 400.
         */
        record $400() {

            /**
             * Factory method creating a result for the status 400 result
             * for the findPetsByTags operation, accepting all the required result values.
             *
             * @return new result data for status 400
             */
            static $400 create() {
                return new $400();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
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
     * Responses for operation {@code getPetById } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface GetPetByIdResult {

        /**
         * Result for HTTP status 200.
         *
         * @param response 
         */
        record $200(Pet response) {

            /**
             * Factory method creating a result for the status 200 result
             * for the getPetById operation, accepting all the required result values.
             *
             * @param response returned entity
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200(null);
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
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
         * Result for HTTP status 400.
         */
        record $400() {

            /**
             * Factory method creating a result for the status 400 result
             * for the getPetById operation, accepting all the required result values.
             *
             * @return new result data for status 400
             */
            static $400 create() {
                return new $400();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
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
         * Result for HTTP status 404.
         */
        record $404() {

            /**
             * Factory method creating a result for the status 404 result
             * for the getPetById operation, accepting all the required result values.
             *
             * @return new result data for status 404
             */
            static $404 create() {
                return new $404();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
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
     * Responses for operation {@code updatePet } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface UpdatePetResult {

        /**
         * Result for HTTP status 200.
         */
        record $200() {

            /**
             * Factory method creating a result for the status 200 result
             * for the updatePet operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
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
         * Result for HTTP status 400.
         */
        record $400() {

            /**
             * Factory method creating a result for the status 400 result
             * for the updatePet operation, accepting all the required result values.
             *
             * @return new result data for status 400
             */
            static $400 create() {
                return new $400();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
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
         * Result for HTTP status 404.
         */
        record $404() {

            /**
             * Factory method creating a result for the status 404 result
             * for the updatePet operation, accepting all the required result values.
             *
             * @return new result data for status 404
             */
            static $404 create() {
                return new $404();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
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
         * Result for HTTP status 405.
         */
        record $405() {

            /**
             * Factory method creating a result for the status 405 result
             * for the updatePet operation, accepting all the required result values.
             *
             * @return new result data for status 405
             */
            static $405 create() {
                return new $405();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
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
     * Responses for operation {@code updatePetWithForm } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface UpdatePetWithFormResult {

        /**
         * Result for HTTP status 200.
         */
        record $200() {

            /**
             * Factory method creating a result for the status 200 result
             * for the updatePetWithForm operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
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
         * Result for HTTP status 405.
         */
        record $405() {

            /**
             * Factory method creating a result for the status 405 result
             * for the updatePetWithForm operation, accepting all the required result values.
             *
             * @return new result data for status 405
             */
            static $405 create() {
                return new $405();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
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
     * Responses for operation {@code uploadFile } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface UploadFileResult {

        /**
         * Result for HTTP status 200.
         *
         * @param response 
         */
        record $200(ModelApiResponse response) {

            /**
             * Factory method creating a result for the status 200 result
             * for the uploadFile operation, accepting all the required result values.
             *
             * @param response returned entity
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200(null);
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
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
     * Responses for operation {@code uploadFileWithRequiredFile } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface UploadFileWithRequiredFileResult {

        /**
         * Result for HTTP status 200.
         *
         * @param response 
         */
        record $200(ModelApiResponse response) {

            /**
             * Factory method creating a result for the status 200 result
             * for the uploadFileWithRequiredFile operation, accepting all the required result values.
             *
             * @param response returned entity
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200(null);
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
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

}
