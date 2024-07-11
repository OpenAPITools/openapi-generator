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
     * Helper elements for the createUser operation.
     */
    static protected class CreateUser {

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
         * Responses for operation {@code createUser} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Default result.
             *
             * @param status (required) Status value to be sent with this default result
             */
            record Default(Status status)     {

                /**
                 * Creates a result builder for the default result
                 * for the createUser operation; there are no required result values for this response.
                 *
                 * @return new builder for status 0
                 */
                static Builder builder(Status status) {
                    return new Builder(status);
                }

                static class Builder implements io.helidon.common.Builder<Builder, Default> {

                    private final Status status;

                    Builder(Status status) {
                        this.status = status;

                    }

                    @Override
                    public Default build() {
                        return new Default(status);
                    }

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
                ValidatorUtils.Validator validator = ValidatorUtils.validator(Logger.getLogger(Result.class.getName()));
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
     * Helper elements for the createUsersWithArrayInput operation.
     */
    static protected class CreateUsersWithArrayInput {

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
         * Responses for operation {@code createUsersWithArrayInput} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Default result.
             *
             * @param status (required) Status value to be sent with this default result
             */
            record Default(Status status)     {

                /**
                 * Creates a result builder for the default result
                 * for the createUsersWithArrayInput operation; there are no required result values for this response.
                 *
                 * @return new builder for status 0
                 */
                static Builder builder(Status status) {
                    return new Builder(status);
                }

                static class Builder implements io.helidon.common.Builder<Builder, Default> {

                    private final Status status;

                    Builder(Status status) {
                        this.status = status;

                    }

                    @Override
                    public Default build() {
                        return new Default(status);
                    }

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
                ValidatorUtils.Validator validator = ValidatorUtils.validator(Logger.getLogger(Result.class.getName()));
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
     * Helper elements for the createUsersWithListInput operation.
     */
    static protected class CreateUsersWithListInput {

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
         * Responses for operation {@code createUsersWithListInput} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Default result.
             *
             * @param status (required) Status value to be sent with this default result
             */
            record Default(Status status)     {

                /**
                 * Creates a result builder for the default result
                 * for the createUsersWithListInput operation; there are no required result values for this response.
                 *
                 * @return new builder for status 0
                 */
                static Builder builder(Status status) {
                    return new Builder(status);
                }

                static class Builder implements io.helidon.common.Builder<Builder, Default> {

                    private final Status status;

                    Builder(Status status) {
                        this.status = status;

                    }

                    @Override
                    public Default build() {
                        return new Default(status);
                    }

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
                ValidatorUtils.Validator validator = ValidatorUtils.validator(Logger.getLogger(Result.class.getName()));
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
     * Helper elements for the deleteUser operation.
     */
    static protected class DeleteUser {

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
                .asOptional()                .orElse(null);
        }

        /**
         * Responses for operation {@code deleteUser} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 400}.
             */
            record S400()     {

                /**
                 * Creates a result builder for the status {@code 400} result
                 * for the deleteUser operation; there are no required result values for this response.
                 *
                 * @return new builder for status 400
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S400> {


                    @Override
                    public S400 build() {
                        return new S400();
                    }

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
            record S404()     {

                /**
                 * Creates a result builder for the status {@code 404} result
                 * for the deleteUser operation; there are no required result values for this response.
                 *
                 * @return new builder for status 404
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S404> {


                    @Override
                    public S404 build() {
                        return new S404();
                    }

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
     * Helper elements for the getUserByName operation.
     */
    static protected class GetUserByName {

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
                .asOptional()                .orElse(null);
        }

        /**
         * Responses for operation {@code getUserByName} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 200}.
            *
             * @param response 
             */
            record S200(User response)     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the getUserByName operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S200> {

                    private User response;

                    @Override
                    public S200 build() {
                        return new S200(response);
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }

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
            record S400()     {

                /**
                 * Creates a result builder for the status {@code 400} result
                 * for the getUserByName operation; there are no required result values for this response.
                 *
                 * @return new builder for status 400
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S400> {


                    @Override
                    public S400 build() {
                        return new S400();
                    }

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
            record S404()     {

                /**
                 * Creates a result builder for the status {@code 404} result
                 * for the getUserByName operation; there are no required result values for this response.
                 *
                 * @return new builder for status 404
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S404> {


                    @Override
                    public S404 build() {
                        return new S404();
                    }

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
     * Helper elements for the loginUser operation.
     */
    static protected class LoginUser {

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
                .asOptional()                .orElse(null);
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
                .asOptional()                .orElse(null);
        }

        /**
         * Responses for operation {@code loginUser} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 200}.
            *
             * @param xRateLimit calls per hour allowed by the user
             * @param xExpiresAfter date in UTC when token expires
             * @param response 
             */
            record S200(Integer xRateLimit,
                        OffsetDateTime xExpiresAfter,
                        String response)     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the loginUser operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S200> {

                    private Integer xRateLimit;                    private OffsetDateTime xExpiresAfter;                    private String response;

                    @Override
                    public S200 build() {
                        return new S200(xRateLimit,
                                xExpiresAfter,
                                response);
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }

                    Builder xRateLimit(Integer xRateLimit) {
                        this.xRateLimit = xRateLimit;
                        return this;
                    }

                    Builder xExpiresAfter(OffsetDateTime xExpiresAfter) {
                        this.xExpiresAfter = xExpiresAfter;
                        return this;
                    }

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
                    serverResponse.status(Status.create(200));                    if (xRateLimit != null) {
                        serverResponse.header("X-Rate-Limit", xRateLimit.toString());
                    }                    if (xExpiresAfter != null) {
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
            record S400()     {

                /**
                 * Creates a result builder for the status {@code 400} result
                 * for the loginUser operation; there are no required result values for this response.
                 *
                 * @return new builder for status 400
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S400> {


                    @Override
                    public S400 build() {
                        return new S400();
                    }

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
     * Helper elements for the logoutUser operation.
     */
    static protected class LogoutUser {

        /**
         * Responses for operation {@code logoutUser} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Default result.
             *
             * @param status (required) Status value to be sent with this default result
             */
            record Default(Status status)     {

                /**
                 * Creates a result builder for the default result
                 * for the logoutUser operation; there are no required result values for this response.
                 *
                 * @return new builder for status 0
                 */
                static Builder builder(Status status) {
                    return new Builder(status);
                }

                static class Builder implements io.helidon.common.Builder<Builder, Default> {

                    private final Status status;

                    Builder(Status status) {
                        this.status = status;

                    }

                    @Override
                    public Default build() {
                        return new Default(status);
                    }

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
                ValidatorUtils.Validator validator = ValidatorUtils.validator(Logger.getLogger(Result.class.getName()));
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
     * Helper elements for the updateUser operation.
     */
    static protected class UpdateUser {

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
                .asOptional()                .orElse(null);
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
         * Responses for operation {@code updateUser} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 400}.
             */
            record S400()     {

                /**
                 * Creates a result builder for the status {@code 400} result
                 * for the updateUser operation; there are no required result values for this response.
                 *
                 * @return new builder for status 400
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S400> {


                    @Override
                    public S400 build() {
                        return new S400();
                    }

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
            record S404()     {

                /**
                 * Creates a result builder for the status {@code 404} result
                 * for the updateUser operation; there are no required result values for this response.
                 *
                 * @return new builder for status 404
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S404> {


                    @Override
                    public S404 build() {
                        return new S404();
                    }

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
    }


    @Override
    public void afterStop() {
    System.out.println("Service UserService is down. Goodbye!");
    }


}
