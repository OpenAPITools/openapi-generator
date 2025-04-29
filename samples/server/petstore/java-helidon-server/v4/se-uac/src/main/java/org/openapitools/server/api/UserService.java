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

import io.helidon.webserver.http.HttpRules;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import io.helidon.webserver.http.HttpService;

@io.helidon.common.Generated(value = "org.openapitools.codegen.languages.JavaHelidonServerCodegen",
                             trigger = "tag = 'User'",
                             version = "stable")
public abstract class UserService implements HttpService {


    protected static final ObjectMapper MAPPER = JsonProvider.objectMapper();

    protected CreateUserOp createUserOp = createCreateUserOp();
    protected CreateUsersWithArrayInputOp createUsersWithArrayInputOp = createCreateUsersWithArrayInputOp();
    protected CreateUsersWithListInputOp createUsersWithListInputOp = createCreateUsersWithListInputOp();
    protected DeleteUserOp deleteUserOp = createDeleteUserOp();
    protected GetUserByNameOp getUserByNameOp = createGetUserByNameOp();
    protected LoginUserOp loginUserOp = createLoginUserOp();
    protected LogoutUserOp logoutUserOp = createLogoutUserOp();
    protected UpdateUserOp updateUserOp = createUpdateUserOp();


    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    public void routing(HttpRules rules) {
        rules.post("/", this::createUser);
        rules.post("/createWithArray", this::createUsersWithArrayInput);
        rules.post("/createWithList", this::createUsersWithListInput);
        rules.delete("/{username}", this::deleteUser);
        rules.get("/{username}", this::getUserByName);
        rules.get("/login", this::loginUser);
        rules.get("/logout", this::logoutUser);
        rules.put("/{username}", this::updateUser);
    }


    /**
     * POST /user : Create user.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void createUser(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        // Parameter: User
        User user = createUserOp.user(request, validator);
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

        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        // Parameter: User
        List<@Valid User> user = createUsersWithArrayInputOp.user(request, validator);
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

        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        // Parameter: User
        List<@Valid User> user = createUsersWithListInputOp.user(request, validator);
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

        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        // Parameter: username
        String username = deleteUserOp.username(request, validator);

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

        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        // Parameter: username
        String username = getUserByNameOp.username(request, validator);

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

        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        // Parameter: username
        String username = loginUserOp.username(request, validator);

        validator.require("username", username);

        // Parameter: password
        String password = loginUserOp.password(request, validator);

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

        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        // Parameter: username
        String username = updateUserOp.username(request, validator);

        validator.require("username", username);

        // Parameter: User
        User user = updateUserOp.user(request, validator);
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
    protected CreateUserOp createCreateUserOp() {
        return new CreateUserOp();
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
    public static class CreateUserOp {

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
             * Creates a response builder for the default response
             * for the createUser operation; there are no required result values for this response.
             *
             * @return new builder for status 0
             */
            static Builder builder(Status status) {
                return new Builder(status);
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
            * Constructor for a result for the default result
            * for the createUser operation, verifying non-null values for required return data.
            *
            */
            public Default(Status status) {
                ValidatorUtils.Validator validator = ValidatorUtils.validator();
                validator.require("status for default response", status);
                validator.execute();
                this.status = status;
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(status);
                _serverResponse.send();
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
    protected CreateUsersWithArrayInputOp createCreateUsersWithArrayInputOp() {
        return new CreateUsersWithArrayInputOp();
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
    public static class CreateUsersWithArrayInputOp {

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
             * Creates a response builder for the default response
             * for the createUsersWithArrayInput operation; there are no required result values for this response.
             *
             * @return new builder for status 0
             */
            static Builder builder(Status status) {
                return new Builder(status);
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
            * Constructor for a result for the default result
            * for the createUsersWithArrayInput operation, verifying non-null values for required return data.
            *
            */
            public Default(Status status) {
                ValidatorUtils.Validator validator = ValidatorUtils.validator();
                validator.require("status for default response", status);
                validator.execute();
                this.status = status;
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(status);
                _serverResponse.send();
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
    protected CreateUsersWithListInputOp createCreateUsersWithListInputOp() {
        return new CreateUsersWithListInputOp();
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
    public static class CreateUsersWithListInputOp {

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
             * Creates a response builder for the default response
             * for the createUsersWithListInput operation; there are no required result values for this response.
             *
             * @return new builder for status 0
             */
            static Builder builder(Status status) {
                return new Builder(status);
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
            * Constructor for a result for the default result
            * for the createUsersWithListInput operation, verifying non-null values for required return data.
            *
            */
            public Default(Status status) {
                ValidatorUtils.Validator validator = ValidatorUtils.validator();
                validator.require("status for default response", status);
                validator.execute();
                this.status = status;
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(status);
                _serverResponse.send();
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
    protected DeleteUserOp createDeleteUserOp() {
        return new DeleteUserOp();
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
    public static class DeleteUserOp {

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
         * Response for HTTP status code {@code 400}.
         */
        record Response400() {

            /**
             * Creates a response builder for the status {@code 400} response
             * for the deleteUser operation; there are no required result values for this response.
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
             * for the deleteUser operation; there are no required result values for this response.
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
     * Returns a new instance of the class which handles parameters to and responses from the getUserByName operation.
     * <p>
     *     Developers can override this method if they extend the UserService class.
     * </p>
     *
     * @return new GetUserByName
     */
    protected GetUserByNameOp createGetUserByNameOp() {
        return new GetUserByNameOp();
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
    public static class GetUserByNameOp {

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
         * Response for HTTP status code {@code 200}.
        *
         * @param response 
         */
        record Response200(User response) {

            /**
             * Creates a response builder for the status {@code 200} response
             * for the getUserByName operation; there are no required result values for this response.
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

                private User response;
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
                Builder response(User response) {
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
             * for the getUserByName operation; there are no required result values for this response.
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
             * for the getUserByName operation; there are no required result values for this response.
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
     * Returns a new instance of the class which handles parameters to and responses from the loginUser operation.
     * <p>
     *     Developers can override this method if they extend the UserService class.
     * </p>
     *
     * @return new LoginUser
     */
    protected LoginUserOp createLoginUserOp() {
        return new LoginUserOp();
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
    public static class LoginUserOp {

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
         * Response for HTTP status code {@code 200}.
        *
         * @param xRateLimit calls per hour allowed by the user
         * @param xExpiresAfter date in UTC when token expires
         * @param response 
         */
        record Response200(Integer xRateLimit,
                    OffsetDateTime xExpiresAfter,
                    String response) {

            /**
             * Creates a response builder for the status {@code 200} response
             * for the loginUser operation; there are no required result values for this response.
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

                private Integer xRateLimit;                private OffsetDateTime xExpiresAfter;                private String response;
                @Override
                public Response200 build() {
                    return new Response200(xRateLimit,
                            xExpiresAfter,
                            response);
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
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(Status.OK_200);                if (xRateLimit != null) {
                    _serverResponse.header("X-Rate-Limit", xRateLimit.toString());
                }                if (xExpiresAfter != null) {
                    _serverResponse.header("X-Expires-After", xExpiresAfter.toString());
                }
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
             * for the loginUser operation; there are no required result values for this response.
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
     * Returns a new instance of the class which handles parameters to and responses from the logoutUser operation.
     * <p>
     *     Developers can override this method if they extend the UserService class.
     * </p>
     *
     * @return new LogoutUser
     */
    protected LogoutUserOp createLogoutUserOp() {
        return new LogoutUserOp();
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
    public static class LogoutUserOp {

        /**
         * Default result.
         *
         * @param status (required) Status value to be sent with this default result
         */
        record Default(Status status) {

            /**
             * Creates a response builder for the default response
             * for the logoutUser operation; there are no required result values for this response.
             *
             * @return new builder for status 0
             */
            static Builder builder(Status status) {
                return new Builder(status);
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
            * Constructor for a result for the default result
            * for the logoutUser operation, verifying non-null values for required return data.
            *
            */
            public Default(Status status) {
                ValidatorUtils.Validator validator = ValidatorUtils.validator();
                validator.require("status for default response", status);
                validator.execute();
                this.status = status;
            }

            /**
             * Applies this response data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
             * HTTP status, any response headers, and any response entity.
             *
             * @param _serverResponse the server response to which to apply these result values
             */
            void send(ServerResponse _serverResponse) {
                _serverResponse.status(status);
                _serverResponse.send();
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
    protected UpdateUserOp createUpdateUserOp() {
        return new UpdateUserOp();
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
    public static class UpdateUserOp {

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
         * Response for HTTP status code {@code 400}.
         */
        record Response400() {

            /**
             * Creates a response builder for the status {@code 400} response
             * for the updateUser operation; there are no required result values for this response.
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
             * for the updateUser operation; there are no required result values for this response.
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


    @Override
    public void afterStop() {
    System.out.println("Service UserService is down. Goodbye!");
    }


}
