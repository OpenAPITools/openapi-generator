package io.swagger.client.api.rxjava;

import io.swagger.client.model.User;

import java.util.*;

import rx.Single;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;


public class UserApi {

	private final io.swagger.client.api.UserApi delegate;

	public UserApi(io.swagger.client.api.UserApi delegate) {
	    this.delegate = delegate;
    }

	public io.swagger.client.api.UserApi getDelegate() {
	    return delegate;
	}

    /**
     * Create user
     * This can only be done by the logged in user.
     * @param body Created user object (required)
     * @param resultHandler Asynchronous result handler
     */
    public void createUser(User body, Handler<AsyncResult<Void>> resultHandler) {
        delegate.createUser(body, resultHandler);
    }

    /**
     * Create user
     * This can only be done by the logged in user.
     * @param body Created user object (required)
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxCreateUser(User body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.createUser(body, fut);
        }));
    }
    /**
     * Creates list of users with given input array
     * 
     * @param body List of user object (required)
     * @param resultHandler Asynchronous result handler
     */
    public void createUsersWithArrayInput(List<User> body, Handler<AsyncResult<Void>> resultHandler) {
        delegate.createUsersWithArrayInput(body, resultHandler);
    }

    /**
     * Creates list of users with given input array
     * 
     * @param body List of user object (required)
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxCreateUsersWithArrayInput(List<User> body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.createUsersWithArrayInput(body, fut);
        }));
    }
    /**
     * Creates list of users with given input array
     * 
     * @param body List of user object (required)
     * @param resultHandler Asynchronous result handler
     */
    public void createUsersWithListInput(List<User> body, Handler<AsyncResult<Void>> resultHandler) {
        delegate.createUsersWithListInput(body, resultHandler);
    }

    /**
     * Creates list of users with given input array
     * 
     * @param body List of user object (required)
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxCreateUsersWithListInput(List<User> body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.createUsersWithListInput(body, fut);
        }));
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
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxDeleteUser(String username) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.deleteUser(username, fut);
        }));
    }
    /**
     * Get user by user name
     * 
     * @param username The name that needs to be fetched. Use user1 for testing.  (required)
     * @param resultHandler Asynchronous result handler
     */
    public void getUserByName(String username, Handler<AsyncResult<User>> resultHandler) {
        delegate.getUserByName(username, resultHandler);
    }

    /**
     * Get user by user name
     * 
     * @param username The name that needs to be fetched. Use user1 for testing.  (required)
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<User> rxGetUserByName(String username) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.getUserByName(username, fut);
        }));
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
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<String> rxLoginUser(String username, String password) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.loginUser(username, password, fut);
        }));
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
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxLogoutUser() {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.logoutUser(fut);
        }));
    }
    /**
     * Updated user
     * This can only be done by the logged in user.
     * @param username name that need to be deleted (required)
     * @param body Updated user object (required)
     * @param resultHandler Asynchronous result handler
     */
    public void updateUser(String username, User body, Handler<AsyncResult<Void>> resultHandler) {
        delegate.updateUser(username, body, resultHandler);
    }

    /**
     * Updated user
     * This can only be done by the logged in user.
     * @param username name that need to be deleted (required)
     * @param body Updated user object (required)
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxUpdateUser(String username, User body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.updateUser(username, body, fut);
        }));
    }

    public static UserApi newInstance(io.swagger.client.api.UserApi arg) {
        return arg != null ? new UserApi(arg) : null;
    }
}
