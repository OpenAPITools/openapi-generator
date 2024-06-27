package org.openapitools.server.api;

import io.helidon.common.GenericType;
import java.util.List;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.OffsetDateTime;
import java.util.Optional;
import io.helidon.http.Status;
import org.openapitools.server.model.User;
import 
import io.helidon.common.Errors;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;
import jakarta.validation.ValidationException;
import org.openapitools.server.model.GenericTypes;

public class UserServiceImpl implements UserService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final System.Logger LOGGER = System.getLogger(UserService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    public void createUser(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        User user = request.content().as(User.class);

        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void createUsersWithArrayInput(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        List<@Valid User> user = request.content().as(GenericTypes.TYPE__List_User);

        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void createUsersWithListInput(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        List<@Valid User> user = request.content().as(GenericTypes.TYPE__List_User);

        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void deleteUser(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        String username = request.path()
                .pathParameters()
                .first("username")
                .asOptional()
                .map(v -> validator.require("username", v))
                .orElse(null);
        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void getUserByName(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        String username = request.path()
                .pathParameters()
                .first("username")
                .asOptional()
                .map(v -> validator.require("username", v))
                .orElse(null);
        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void loginUser(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        String username = request.query()
                .first("username")
                .asOptional()
                .map(v -> validator.require("username", v))
                .orElse(null);
        String password = request.query()
                .first("password")
                .asOptional()
                .map(v -> validator.require("password", v))
                .orElse(null);
        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void logoutUser(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void updateUser(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        String username = request.path()
                .pathParameters()
                .first("username")
                .asOptional()
                .map(v -> validator.require("username", v))
                .orElse(null);
        User user = request.content().as(User.class);

        validator.execute();
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void afterStop() {
        System.out.println("Service UserService is down. Goodbye!");
    }




    /**
     * Responses for operation {@code createUser } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface CreateUserResult {

        /**
         * Default result.
         *
         * @param status (required) Status value to be sent with this default result
         */
        record Default(Status status) {

            /**
             * Creates a result for the default result
             * for the createUser operation, accepting all the required result values.
             *
             * @return new result data for status 0
             */
            static Default create(Status status) {
                return new Default(status);
            }

           /**
            * Constructor for a result for the default result
            * for the createUser operation, verifying non-null values for required return data.
            *
            */
            public Default(Status status) {
                ValidatorUtils.Validator validator = ValidatorUtils.validator(System.getLogger(CreateUserResult.class.getName());
                validator.require("status for default response", status);
                this.status = status;
                validator.execute();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(status);
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code createUsersWithArrayInput } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface CreateUsersWithArrayInputResult {

        /**
         * Default result.
         *
         * @param status (required) Status value to be sent with this default result
         */
        record Default(Status status) {

            /**
             * Creates a result for the default result
             * for the createUsersWithArrayInput operation, accepting all the required result values.
             *
             * @return new result data for status 0
             */
            static Default create(Status status) {
                return new Default(status);
            }

           /**
            * Constructor for a result for the default result
            * for the createUsersWithArrayInput operation, verifying non-null values for required return data.
            *
            */
            public Default(Status status) {
                ValidatorUtils.Validator validator = ValidatorUtils.validator(System.getLogger(CreateUsersWithArrayInputResult.class.getName());
                validator.require("status for default response", status);
                this.status = status;
                validator.execute();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(status);
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code createUsersWithListInput } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface CreateUsersWithListInputResult {

        /**
         * Default result.
         *
         * @param status (required) Status value to be sent with this default result
         */
        record Default(Status status) {

            /**
             * Creates a result for the default result
             * for the createUsersWithListInput operation, accepting all the required result values.
             *
             * @return new result data for status 0
             */
            static Default create(Status status) {
                return new Default(status);
            }

           /**
            * Constructor for a result for the default result
            * for the createUsersWithListInput operation, verifying non-null values for required return data.
            *
            */
            public Default(Status status) {
                ValidatorUtils.Validator validator = ValidatorUtils.validator(System.getLogger(CreateUsersWithListInputResult.class.getName());
                validator.require("status for default response", status);
                this.status = status;
                validator.execute();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(status);
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code deleteUser } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface DeleteUserResult {

        /**
         * Result for HTTP status 400.
         */
        record $400() {

            /**
             * Creates a result for the status 400 result
             * for the deleteUser operation, accepting all the required result values.
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
             * Creates a result for the status 404 result
             * for the deleteUser operation, accepting all the required result values.
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
     * Responses for operation {@code getUserByName } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface GetUserByNameResult {

        /**
         * Result for HTTP status 200.
         *
         * @param response 
         */
        record $200(User response) {

            /**
             * Creates a result for the status 200 result
             * for the getUserByName operation, accepting all the required result values.
             *
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
             * Creates a result for the status 400 result
             * for the getUserByName operation, accepting all the required result values.
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
             * Creates a result for the status 404 result
             * for the getUserByName operation, accepting all the required result values.
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
     * Responses for operation {@code loginUser } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface LoginUserResult {

        /**
         * Result for HTTP status 200.
         *
         * @param xRateLimit calls per hour allowed by the user
         * @param xExpiresAfter date in UTC when token expires
         * @param response 
         */
        record $200(Integer xRateLimit,
                    OffsetDateTime xExpiresAfter,
                    String response) {

            /**
             * Creates a result for the status 200 result
             * for the loginUser operation, accepting all the required result values.
             *
             * @return new result data for status 200
             */
            static $200 create() {
                return new $200(null,
                        null,
                        null);
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(Status.create(200));
                if (xRateLimit != null) {
                    serverResponse.header("X-Rate-Limit", xRateLimit.toString());
                }
                if (xExpiresAfter != null) {
                    serverResponse.header("X-Expires-After", xExpiresAfter.toString());
                }
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
             * Creates a result for the status 400 result
             * for the loginUser operation, accepting all the required result values.
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
     * Responses for operation {@code logoutUser } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface LogoutUserResult {

        /**
         * Default result.
         *
         * @param status (required) Status value to be sent with this default result
         */
        record Default(Status status) {

            /**
             * Creates a result for the default result
             * for the logoutUser operation, accepting all the required result values.
             *
             * @return new result data for status 0
             */
            static Default create(Status status) {
                return new Default(status);
            }

           /**
            * Constructor for a result for the default result
            * for the logoutUser operation, verifying non-null values for required return data.
            *
            */
            public Default(Status status) {
                ValidatorUtils.Validator validator = ValidatorUtils.validator(System.getLogger(LogoutUserResult.class.getName());
                validator.require("status for default response", status);
                this.status = status;
                validator.execute();
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             * @param serverResponse the server response to which to apply these result values
             * @return the updated server response
             */
            ServerResponse apply(ServerResponse serverResponse) {
                serverResponse.status(status);
                serverResponse.send();
                return serverResponse;
            }
        }
    }

    /**
     * Responses for operation {@code updateUser } organized by response status.
     * <p>
     *     Once your code determines which (if any) response to send, it can use the static {@code create} method and pass
     *     the required elements of the response, then assign any optional response elements using the record fields.
     * <p>
     *     Finally, invoke the {@code apply} method, passing the original {@link ServerResponse}; the method sets any headers
     *     you have assigned, sets the correct status in the response, and sends the response including any appropriate
     *     entity.
     * </p>
     */
    interface UpdateUserResult {

        /**
         * Result for HTTP status 400.
         */
        record $400() {

            /**
             * Creates a result for the status 400 result
             * for the updateUser operation, accepting all the required result values.
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
             * Creates a result for the status 404 result
             * for the updateUser operation, accepting all the required result values.
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

}
