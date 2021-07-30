package org.openapitools.api;

import org.openapitools.model.User;
import io.micronaut.test.extensions.junit5.annotation.MicronautTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import jakarta.inject.Inject;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * API tests for UserApi
 */
@MicronautTest
public class UserApiTest {

    @Inject
    UserApi api;

    
    /**
     * Create user
     *
     * This can only be done by the logged in user.
     */
    @Test
    public void createUserTest() {
        User _body = null;
        // api.createUser(_body).block();
        // Mono<Void> asyncResponse = api.createUser(_body);
        // TODO: test validations
    }

    
    /**
     * Creates list of users with given input array
          */
    @Test
    public void createUsersWithArrayInputTest() {
        List<User> _body = null;
        // api.createUsersWithArrayInput(_body).block();
        // Mono<Void> asyncResponse = api.createUsersWithArrayInput(_body);
        // TODO: test validations
    }

    
    /**
     * Creates list of users with given input array
          */
    @Test
    public void createUsersWithListInputTest() {
        List<User> _body = null;
        // api.createUsersWithListInput(_body).block();
        // Mono<Void> asyncResponse = api.createUsersWithListInput(_body);
        // TODO: test validations
    }

    
    /**
     * Delete user
     *
     * This can only be done by the logged in user.
     */
    @Test
    public void deleteUserTest() {
        String username = null;
        // api.deleteUser(username).block();
        // Mono<Void> asyncResponse = api.deleteUser(username);
        // TODO: test validations
    }

    
    /**
     * Get user by user name
          */
    @Test
    public void getUserByNameTest() {
        String username = null;
        // User response = api.getUserByName(username).block();
        // Mono<User> asyncResponse = api.getUserByName(username);
        // TODO: test validations
    }

    
    /**
     * Logs user into the system
          */
    @Test
    public void loginUserTest() {
        String username = null;
        String password = null;
        // String response = api.loginUser(username, password).block();
        // Mono<String> asyncResponse = api.loginUser(username, password);
        // TODO: test validations
    }

    
    /**
     * Logs out current logged in user session
          */
    @Test
    public void logoutUserTest() {
        // api.logoutUser().block();
        // Mono<Void> asyncResponse = api.logoutUser();
        // TODO: test validations
    }

    
    /**
     * Updated user
     *
     * This can only be done by the logged in user.
     */
    @Test
    public void updateUserTest() {
        String username = null;
        User _body = null;
        // api.updateUser(username, _body).block();
        // Mono<Void> asyncResponse = api.updateUser(username, _body);
        // TODO: test validations
    }

    
}
