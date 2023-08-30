package org.openapitools.client.api.rxjava;

import java.time.OffsetDateTime;
import org.openapitools.client.model.User;
import org.openapitools.client.ApiClient;

import java.util.*;

import rx.Single;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class UserApi {

    private final org.openapitools.client.api.UserApi delegate;

    public UserApi(org.openapitools.client.api.UserApi delegate) {
        this.delegate = delegate;
    }

    public org.openapitools.client.api.UserApi getDelegate() {
        return delegate;
    }

    /**
    * Create user
    * This can only be done by the logged in user.
    * @param user Created user object (required)
    * @param resultHandler Asynchronous result handler
    */
    public void createUser(User user, Handler<AsyncResult<Void>> resultHandler) {
        delegate.createUser(user, resultHandler);
    }

    /**
    * Create user
    * This can only be done by the logged in user.
    * @param user Created user object (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void createUser(User user, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.createUser(user, authInfo, resultHandler);
    }

    /**
    * Create user
    * This can only be done by the logged in user.
    * @param user Created user object (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxCreateUser(User user) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.createUser(user, fut)
        ));
    }

    /**
    * Create user
    * This can only be done by the logged in user.
    * @param user Created user object (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxCreateUser(User user, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.createUser(user, authInfo, fut)
        ));
    }
    /**
    * Creates list of users with given input array
    * 
    * @param user List of user object (required)
    * @param resultHandler Asynchronous result handler
    */
    public void createUsersWithArrayInput(List<User> user, Handler<AsyncResult<Void>> resultHandler) {
        delegate.createUsersWithArrayInput(user, resultHandler);
    }

    /**
    * Creates list of users with given input array
    * 
    * @param user List of user object (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void createUsersWithArrayInput(List<User> user, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.createUsersWithArrayInput(user, authInfo, resultHandler);
    }

    /**
    * Creates list of users with given input array
    * 
    * @param user List of user object (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxCreateUsersWithArrayInput(List<User> user) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.createUsersWithArrayInput(user, fut)
        ));
    }

    /**
    * Creates list of users with given input array
    * 
    * @param user List of user object (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxCreateUsersWithArrayInput(List<User> user, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.createUsersWithArrayInput(user, authInfo, fut)
        ));
    }
    /**
    * Creates list of users with given input array
    * 
    * @param user List of user object (required)
    * @param resultHandler Asynchronous result handler
    */
    public void createUsersWithListInput(List<User> user, Handler<AsyncResult<Void>> resultHandler) {
        delegate.createUsersWithListInput(user, resultHandler);
    }

    /**
    * Creates list of users with given input array
    * 
    * @param user List of user object (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void createUsersWithListInput(List<User> user, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.createUsersWithListInput(user, authInfo, resultHandler);
    }

    /**
    * Creates list of users with given input array
    * 
    * @param user List of user object (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxCreateUsersWithListInput(List<User> user) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.createUsersWithListInput(user, fut)
        ));
    }

    /**
    * Creates list of users with given input array
    * 
    * @param user List of user object (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxCreateUsersWithListInput(List<User> user, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.createUsersWithListInput(user, authInfo, fut)
        ));
    }
    /**
    * Delete user
    * This can only be done by the logged in user.
    * @param username The name that needs to be deleted (required)
    * @param resultHandler Asynchronous result handler
    */
    public void deleteUser(String username, Handler<AsyncResult<Void>> resultHandler) {
        delegate.deleteUser(username, resultHandler);
    }

    /**
    * Delete user
    * This can only be done by the logged in user.
    * @param username The name that needs to be deleted (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void deleteUser(String username, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.deleteUser(username, authInfo, resultHandler);
    }

    /**
    * Delete user
    * This can only be done by the logged in user.
    * @param username The name that needs to be deleted (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxDeleteUser(String username) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.deleteUser(username, fut)
        ));
    }

    /**
    * Delete user
    * This can only be done by the logged in user.
    * @param username The name that needs to be deleted (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxDeleteUser(String username, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.deleteUser(username, authInfo, fut)
        ));
    }
    /**
    * Get user by user name
    * 
    * @param username The name that needs to be fetched. Use user1 for testing. (required)
    * @param resultHandler Asynchronous result handler
    */
    public void getUserByName(String username, Handler<AsyncResult<User>> resultHandler) {
        delegate.getUserByName(username, resultHandler);
    }

    /**
    * Get user by user name
    * 
    * @param username The name that needs to be fetched. Use user1 for testing. (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void getUserByName(String username, ApiClient.AuthInfo authInfo, Handler<AsyncResult<User>> resultHandler) {
        delegate.getUserByName(username, authInfo, resultHandler);
    }

    /**
    * Get user by user name
    * 
    * @param username The name that needs to be fetched. Use user1 for testing. (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<User> rxGetUserByName(String username) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.getUserByName(username, fut)
        ));
    }

    /**
    * Get user by user name
    * 
    * @param username The name that needs to be fetched. Use user1 for testing. (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<User> rxGetUserByName(String username, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.getUserByName(username, authInfo, fut)
        ));
    }
    /**
    * Logs user into the system
    * 
    * @param username The user name for login (required)
    * @param password The password for login in clear text (required)
    * @param resultHandler Asynchronous result handler
    */
    public void loginUser(String username, String password, Handler<AsyncResult<String>> resultHandler) {
        delegate.loginUser(username, password, resultHandler);
    }

    /**
    * Logs user into the system
    * 
    * @param username The user name for login (required)
    * @param password The password for login in clear text (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void loginUser(String username, String password, ApiClient.AuthInfo authInfo, Handler<AsyncResult<String>> resultHandler) {
        delegate.loginUser(username, password, authInfo, resultHandler);
    }

    /**
    * Logs user into the system
    * 
    * @param username The user name for login (required)
    * @param password The password for login in clear text (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<String> rxLoginUser(String username, String password) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.loginUser(username, password, fut)
        ));
    }

    /**
    * Logs user into the system
    * 
    * @param username The user name for login (required)
    * @param password The password for login in clear text (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<String> rxLoginUser(String username, String password, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.loginUser(username, password, authInfo, fut)
        ));
    }
    /**
    * Logs out current logged in user session
    * 
    * @param resultHandler Asynchronous result handler
    */
    public void logoutUser(Handler<AsyncResult<Void>> resultHandler) {
        delegate.logoutUser(resultHandler);
    }

    /**
    * Logs out current logged in user session
    * 
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void logoutUser(ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.logoutUser(authInfo, resultHandler);
    }

    /**
    * Logs out current logged in user session
    * 
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxLogoutUser() {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.logoutUser(fut)
        ));
    }

    /**
    * Logs out current logged in user session
    * 
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxLogoutUser(ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.logoutUser(authInfo, fut)
        ));
    }
    /**
    * Updated user
    * This can only be done by the logged in user.
    * @param username name that need to be deleted (required)
    * @param user Updated user object (required)
    * @param resultHandler Asynchronous result handler
    */
    public void updateUser(String username, User user, Handler<AsyncResult<Void>> resultHandler) {
        delegate.updateUser(username, user, resultHandler);
    }

    /**
    * Updated user
    * This can only be done by the logged in user.
    * @param username name that need to be deleted (required)
    * @param user Updated user object (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void updateUser(String username, User user, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.updateUser(username, user, authInfo, resultHandler);
    }

    /**
    * Updated user
    * This can only be done by the logged in user.
    * @param username name that need to be deleted (required)
    * @param user Updated user object (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxUpdateUser(String username, User user) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.updateUser(username, user, fut)
        ));
    }

    /**
    * Updated user
    * This can only be done by the logged in user.
    * @param username name that need to be deleted (required)
    * @param user Updated user object (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxUpdateUser(String username, User user, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.updateUser(username, user, authInfo, fut)
        ));
    }

    public static UserApi newInstance(org.openapitools.client.api.UserApi arg) {
        return arg != null ? new UserApi(arg) : null;
    }
}
