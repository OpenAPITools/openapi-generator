package org.openapitools.server.api;

import java.util.stream.Collectors;
import org.openapitools.server.model.GenericTypes;
import java.util.HexFormat;
import java.util.List;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.OffsetDateTime;
import java.util.Optional;
import io.helidon.http.Status;
import org.openapitools.server.model.User;
import jakarta.validation.Valid;

import java.util.Optional;
import java.util.logging.Logger;

import io.helidon.webserver.http.HttpRules;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import io.helidon.webserver.http.HttpService;

@io.helidon.common.Generated(value = "org.openapitools.codegen.languages.JavaHelidonServerCodegen",
                             trigger = "tag = 'User'",
                             version = "7.8.0-SNAPSHOT")
public abstract class UserService implements HttpService {

    protected static final Logger LOGGER = Logger.getLogger(UserService.class.getName());
    protected static final ObjectMapper MAPPER = JsonProvider.objectMapper();

    protected CreateUser createUser = createUser();
    protected CreateUsersWithArrayInput createUsersWithArrayInput = createUsersWithArrayInput();
    protected CreateUsersWithListInput createUsersWithListInput = createUsersWithListInput();
    protected DeleteUser deleteUser = deleteUser();
    protected GetUserByName getUserByName = getUserByName();
    protected LoginUser loginUser = loginUser();
    protected LogoutUser logoutUser = logoutUser();
    protected UpdateUser updateUser = updateUser();


    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    public void routing(HttpRules rules) {
        rules.post("/user", this::createUser);
        rules.post("/user/createWithArray", this::createUsersWithArrayInput);
        rules.post("/user/createWithList", this::createUsersWithListInput);
        rules.delete("/user/{username}", this::deleteUser);
        rules.get("/user/{username}", this::getUserByName);
        rules.get("/user/login", this::loginUser);
        rules.get("/user/logout", this::logoutUser);
        rules.put("/user/{username}", this::updateUser);
    }


    /**
     * POST /user : Create user.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void createUser(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: User
        User user = createUser.user(request, validator);
        validator.require("user", user);

        validator.execute();

        handleCreateUser(request, response, 
                    user);
    }

    /**
     * Handle POST /user : Create user.
     *
     * @param request the server request
     * @param response the server response
     * @param user Created user object 
     */
    protected abstract void handleCreateUser(ServerRequest request, ServerResponse response, 
                User user);

    /**
     * POST /user/createWithArray : Creates list of users with given input array.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void createUsersWithArrayInput(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: User
        List<@Valid User> user = createUsersWithArrayInput.user(request, validator);
        validator.require("user", user);

        validator.execute();

        handleCreateUsersWithArrayInput(request, response, 
                    user);
    }

    /**
     * Handle POST /user/createWithArray : Creates list of users with given input array.
     *
     * @param request the server request
     * @param response the server response
     * @param user List of user object 
     */
    protected abstract void handleCreateUsersWithArrayInput(ServerRequest request, ServerResponse response, 
                List<@Valid User> user);

    /**
     * POST /user/createWithList : Creates list of users with given input array.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void createUsersWithListInput(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: User
        List<@Valid User> user = createUsersWithListInput.user(request, validator);
        validator.require("user", user);

        validator.execute();

        handleCreateUsersWithListInput(request, response, 
                    user);
    }

    /**
     * Handle POST /user/createWithList : Creates list of users with given input array.
     *
     * @param request the server request
     * @param response the server response
     * @param user List of user object 
     */
    protected abstract void handleCreateUsersWithListInput(ServerRequest request, ServerResponse response, 
                List<@Valid User> user);

    /**
     * DELETE /user/{username} : Delete user.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void deleteUser(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: username
        String username = deleteUser.username(request, validator);

        validator.require("username", username);
        validator.execute();

        handleDeleteUser(request, response, 
                    username);
    }

    /**
     * Handle DELETE /user/{username} : Delete user.
     *
     * @param request the server request
     * @param response the server response
     * @param username The name that needs to be deleted 
     */
    protected abstract void handleDeleteUser(ServerRequest request, ServerResponse response, 
                String username);

    /**
     * GET /user/{username} : Get user by user name.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void getUserByName(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: username
        String username = getUserByName.username(request, validator);

        validator.require("username", username);
        validator.execute();

        handleGetUserByName(request, response, 
                    username);
    }

    /**
     * Handle GET /user/{username} : Get user by user name.
     *
     * @param request the server request
     * @param response the server response
     * @param username The name that needs to be fetched. Use user1 for testing. 
     */
    protected abstract void handleGetUserByName(ServerRequest request, ServerResponse response, 
                String username);

    /**
     * GET /user/login : Logs user into the system.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void loginUser(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: username
        String username = loginUser.username(request, validator);

        validator.require("username", username);

        // Parameter: password
        String password = loginUser.password(request, validator);

        validator.require("password", password);
        validator.execute();

        handleLoginUser(request, response, 
                    username, 
                    password);
    }

    /**
     * Handle GET /user/login : Logs user into the system.
     *
     * @param request the server request
     * @param response the server response
     * @param username The user name for login 
     * @param password The password for login in clear text 
     */
    protected abstract void handleLoginUser(ServerRequest request, ServerResponse response, 
                String username, 
                String password);

    /**
     * GET /user/logout : Logs out current logged in user session.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void logoutUser(ServerRequest request, ServerResponse response) { 

                handleLogoutUser(request, response);
    }

    /**
     * Handle GET /user/logout : Logs out current logged in user session.
     *
     * @param request the server request
     * @param response the server response
     */
    protected abstract void handleLogoutUser(ServerRequest request, ServerResponse response);

    /**
     * PUT /user/{username} : Updated user.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void updateUser(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: username
        String username = updateUser.username(request, validator);

        validator.require("username", username);

        // Parameter: User
        User user = updateUser.user(request, validator);
        validator.require("user", user);

        validator.execute();

        handleUpdateUser(request, response, 
                    username, 
                    user);
    }

    /**
     * Handle PUT /user/{username} : Updated user.
     *
     * @param request the server request
     * @param response the server response
     * @param username name that need to be deleted 
     * @param user Updated user object 
     */
    protected abstract void handleUpdateUser(ServerRequest request, ServerResponse response, 
                String username, 
                User user);

    /**
     * Returns a new instance of the class which handles parameters to and responses from the createUser operation.
     * <p>
     *     Developers can override this method if they extend the UserService class.
     * </p>
     *
     * @return new CreateUser
     */
    protected CreateUser createUser() {
        return new CreateUser();
    }

    /**
     * Helper elements for the {@code createUser} operation.
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
    public static class CreateUser {

        /**
         * Prepares the user parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return user parameter value
         */
        protected User user(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? request.content().as(User.class)
                : null;
        }

        /**
         * Default result.
         *
         * @param status (required) Status value to be sent with this default result
         */
        record Default(Status status) {

            /**
             * Creates a result builder for the default result
             * for the createUser operation; there are no required result values for this response.
             *
             * @return new builder for status 0
             */
            static Builder builder(Status status) {
                return new Builder(status);
            }

            /**
             * Applies the required response parameters to the server response and sends the response.
             *
             * status HTTP Status object to use for the response status
             */
            static void send(ServerResponse serverResponse, Status status) {
                builder(status).apply(serverResponse);
            }

            /**
             * Builder for the Default result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Default> {

                private final Status status;

                Builder(Status status) {
                    this.status = status;

                }

                @Override
                public Default build() {
                    return new Default(status);
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
            * Constructor for a result for the default result
            * for the createUser operation, verifying non-null values for required return data.
            *
            */
            public Default(Status status) {
                ValidatorUtils.Validator validator = ValidatorUtils.validator(Logger.getLogger(CreateUser.class.getName()));
                validator.require("status for default response", status);
                validator.execute();
                this.status = status;
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
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
     * Returns a new instance of the class which handles parameters to and responses from the createUsersWithArrayInput operation.
     * <p>
     *     Developers can override this method if they extend the UserService class.
     * </p>
     *
     * @return new CreateUsersWithArrayInput
     */
    protected CreateUsersWithArrayInput createUsersWithArrayInput() {
        return new CreateUsersWithArrayInput();
    }

    /**
     * Helper elements for the {@code createUsersWithArrayInput} operation.
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
    public static class CreateUsersWithArrayInput {

        /**
         * Prepares the user parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return user parameter value
         */
        protected List<@Valid User> user(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? request.content().as(GenericTypes.TYPE__List_User)
                : List.of();
        }

        /**
         * Default result.
         *
         * @param status (required) Status value to be sent with this default result
         */
        record Default(Status status) {

            /**
             * Creates a result builder for the default result
             * for the createUsersWithArrayInput operation; there are no required result values for this response.
             *
             * @return new builder for status 0
             */
            static Builder builder(Status status) {
                return new Builder(status);
            }

            /**
             * Applies the required response parameters to the server response and sends the response.
             *
             * status HTTP Status object to use for the response status
             */
            static void send(ServerResponse serverResponse, Status status) {
                builder(status).apply(serverResponse);
            }

            /**
             * Builder for the Default result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Default> {

                private final Status status;

                Builder(Status status) {
                    this.status = status;

                }

                @Override
                public Default build() {
                    return new Default(status);
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
            * Constructor for a result for the default result
            * for the createUsersWithArrayInput operation, verifying non-null values for required return data.
            *
            */
            public Default(Status status) {
                ValidatorUtils.Validator validator = ValidatorUtils.validator(Logger.getLogger(CreateUsersWithArrayInput.class.getName()));
                validator.require("status for default response", status);
                validator.execute();
                this.status = status;
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
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
     * Returns a new instance of the class which handles parameters to and responses from the createUsersWithListInput operation.
     * <p>
     *     Developers can override this method if they extend the UserService class.
     * </p>
     *
     * @return new CreateUsersWithListInput
     */
    protected CreateUsersWithListInput createUsersWithListInput() {
        return new CreateUsersWithListInput();
    }

    /**
     * Helper elements for the {@code createUsersWithListInput} operation.
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
    public static class CreateUsersWithListInput {

        /**
         * Prepares the user parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return user parameter value
         */
        protected List<@Valid User> user(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? request.content().as(GenericTypes.TYPE__List_User)
                : List.of();
        }

        /**
         * Default result.
         *
         * @param status (required) Status value to be sent with this default result
         */
        record Default(Status status) {

            /**
             * Creates a result builder for the default result
             * for the createUsersWithListInput operation; there are no required result values for this response.
             *
             * @return new builder for status 0
             */
            static Builder builder(Status status) {
                return new Builder(status);
            }

            /**
             * Applies the required response parameters to the server response and sends the response.
             *
             * status HTTP Status object to use for the response status
             */
            static void send(ServerResponse serverResponse, Status status) {
                builder(status).apply(serverResponse);
            }

            /**
             * Builder for the Default result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Default> {

                private final Status status;

                Builder(Status status) {
                    this.status = status;

                }

                @Override
                public Default build() {
                    return new Default(status);
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
            * Constructor for a result for the default result
            * for the createUsersWithListInput operation, verifying non-null values for required return data.
            *
            */
            public Default(Status status) {
                ValidatorUtils.Validator validator = ValidatorUtils.validator(Logger.getLogger(CreateUsersWithListInput.class.getName()));
                validator.require("status for default response", status);
                validator.execute();
                this.status = status;
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
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
     * Returns a new instance of the class which handles parameters to and responses from the deleteUser operation.
     * <p>
     *     Developers can override this method if they extend the UserService class.
     * </p>
     *
     * @return new DeleteUser
     */
    protected DeleteUser deleteUser() {
        return new DeleteUser();
    }

    /**
     * Helper elements for the {@code deleteUser} operation.
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
    public static class DeleteUser {

        /**
         * Prepares the username parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return username parameter value
         */
        protected String username(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.path()
                .pathParameters()
                .first("username")
                .asOptional()
                .orElse(null);
        }

        /**
         * Result for HTTP status code {@code 400}.
         */
        record result400() {

            /**
             * Creates a result builder for the status {@code 400} result
             * for the deleteUser operation; there are no required result values for this response.
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
             * for the deleteUser operation; there are no required result values for this response.
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
     * Returns a new instance of the class which handles parameters to and responses from the getUserByName operation.
     * <p>
     *     Developers can override this method if they extend the UserService class.
     * </p>
     *
     * @return new GetUserByName
     */
    protected GetUserByName getUserByName() {
        return new GetUserByName();
    }

    /**
     * Helper elements for the {@code getUserByName} operation.
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
    public static class GetUserByName {

        /**
         * Prepares the username parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return username parameter value
         */
        protected String username(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.path()
                .pathParameters()
                .first("username")
                .asOptional()
                .orElse(null);
        }

        /**
         * Result for HTTP status code {@code 200}.
        *
         * @param response 
         */
        record result200(User response) {

            /**
             * Creates a result builder for the status {@code 200} result
             * for the getUserByName operation; there are no required result values for this response.
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

                private User response;

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
                Builder response(User response) {
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
             * for the getUserByName operation; there are no required result values for this response.
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
             * for the getUserByName operation; there are no required result values for this response.
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
     * Returns a new instance of the class which handles parameters to and responses from the loginUser operation.
     * <p>
     *     Developers can override this method if they extend the UserService class.
     * </p>
     *
     * @return new LoginUser
     */
    protected LoginUser loginUser() {
        return new LoginUser();
    }

    /**
     * Helper elements for the {@code loginUser} operation.
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
    public static class LoginUser {

        /**
         * Prepares the username parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return username parameter value
         */
        protected String username(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .first("username")
                .asOptional()
                .orElse(null);
        }

        /**
         * Prepares the password parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return password parameter value
         */
        protected String password(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .first("password")
                .asOptional()
                .orElse(null);
        }

        /**
         * Result for HTTP status code {@code 200}.
        *
         * @param xRateLimit calls per hour allowed by the user
         * @param xExpiresAfter date in UTC when token expires
         * @param response 
         */
        record result200(Integer xRateLimit,
                    OffsetDateTime xExpiresAfter,
                    String response) {

            /**
             * Creates a result builder for the status {@code 200} result
             * for the loginUser operation; there are no required result values for this response.
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

                private Integer xRateLimit;                private OffsetDateTime xExpiresAfter;                private String response;

                @Override
                public result200 build() {
                    return new result200(xRateLimit,
                            xExpiresAfter,
                            response);
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
                 * Sets the value for the optional return property {@code xRateLimit}.
                 * @param xRateLimit calls per hour allowed by the user
                 * @return updated result builder
                 */
                Builder xRateLimit(Integer xRateLimit) {
                    this.xRateLimit = xRateLimit;
                    return this;
                }

                /**
                 * Sets the value for the optional return property {@code xExpiresAfter}.
                 * @param xExpiresAfter date in UTC when token expires
                 * @return updated result builder
                 */
                Builder xExpiresAfter(OffsetDateTime xExpiresAfter) {
                    this.xExpiresAfter = xExpiresAfter;
                    return this;
                }

                /**
                 * Sets the value for the optional return property {@code response}.
                 * @param response 
                 * @return updated result builder
                 */
                Builder response(String response) {
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
                serverResponse.status(Status.create(200));                if (xRateLimit != null) {
                    serverResponse.header("X-Rate-Limit", xRateLimit.toString());
                }                if (xExpiresAfter != null) {
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
         * Result for HTTP status code {@code 400}.
         */
        record result400() {

            /**
             * Creates a result builder for the status {@code 400} result
             * for the loginUser operation; there are no required result values for this response.
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
     * Returns a new instance of the class which handles parameters to and responses from the logoutUser operation.
     * <p>
     *     Developers can override this method if they extend the UserService class.
     * </p>
     *
     * @return new LogoutUser
     */
    protected LogoutUser logoutUser() {
        return new LogoutUser();
    }

    /**
     * Helper elements for the {@code logoutUser} operation.
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
    public static class LogoutUser {

        /**
         * Default result.
         *
         * @param status (required) Status value to be sent with this default result
         */
        record Default(Status status) {

            /**
             * Creates a result builder for the default result
             * for the logoutUser operation; there are no required result values for this response.
             *
             * @return new builder for status 0
             */
            static Builder builder(Status status) {
                return new Builder(status);
            }

            /**
             * Applies the required response parameters to the server response and sends the response.
             *
             * status HTTP Status object to use for the response status
             */
            static void send(ServerResponse serverResponse, Status status) {
                builder(status).apply(serverResponse);
            }

            /**
             * Builder for the Default result.
             */
            static class Builder implements io.helidon.common.Builder<Builder, Default> {

                private final Status status;

                Builder(Status status) {
                    this.status = status;

                }

                @Override
                public Default build() {
                    return new Default(status);
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
            * Constructor for a result for the default result
            * for the logoutUser operation, verifying non-null values for required return data.
            *
            */
            public Default(Status status) {
                ValidatorUtils.Validator validator = ValidatorUtils.validator(Logger.getLogger(LogoutUser.class.getName()));
                validator.require("status for default response", status);
                validator.execute();
                this.status = status;
            }

            /**
             * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
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
     * Returns a new instance of the class which handles parameters to and responses from the updateUser operation.
     * <p>
     *     Developers can override this method if they extend the UserService class.
     * </p>
     *
     * @return new UpdateUser
     */
    protected UpdateUser updateUser() {
        return new UpdateUser();
    }

    /**
     * Helper elements for the {@code updateUser} operation.
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
    public static class UpdateUser {

        /**
         * Prepares the username parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return username parameter value
         */
        protected String username(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.path()
                .pathParameters()
                .first("username")
                .asOptional()
                .orElse(null);
        }

        /**
         * Prepares the user parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return user parameter value
         */
        protected User user(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? request.content().as(User.class)
                : null;
        }

        /**
         * Result for HTTP status code {@code 400}.
         */
        record result400() {

            /**
             * Creates a result builder for the status {@code 400} result
             * for the updateUser operation; there are no required result values for this response.
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
             * for the updateUser operation; there are no required result values for this response.
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


    @Override
    public void afterStop() {
    System.out.println("Service UserService is down. Goodbye!");
    }


}
