package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.model.User;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * API tests for UserApi
 */
class UserApiTest {

    private UserApi api;

    @BeforeEach
    public void setup() {
        api = new ApiClient().buildClient(UserApi.class);
    }

    
    /**
     * Create user
     *
     * This can only be done by the logged in user.
     */
    @Test
    void createUserTest() {
        User body = null;
        // api.createUser(body);

        // TODO: test validations
    }

    
    /**
     * Creates list of users with given input array
     *
     * 
     */
    @Test
    void createUsersWithArrayInputTest() {
        List<User> body = null;
        // api.createUsersWithArrayInput(body);

        // TODO: test validations
    }

    
    /**
     * Creates list of users with given input array
     *
     * 
     */
    @Test
    void createUsersWithListInputTest() {
        List<User> body = null;
        // api.createUsersWithListInput(body);

        // TODO: test validations
    }

    
    /**
     * Delete user
     *
     * This can only be done by the logged in user.
     */
    @Test
    void deleteUserTest() {
        String username = null;
        // api.deleteUser(username);

        // TODO: test validations
    }

    
    /**
     * Get user by user name
     *
     * 
     */
    @Test
    void getUserByNameTest() {
        String username = null;
        // User response = api.getUserByName(username);

        // TODO: test validations
    }

    
    /**
     * Logs user into the system
     *
     * 
     */
    @Test
    void loginUserTest() {
        String username = null;
        String password = null;
        // String response = api.loginUser(username, password);

        // TODO: test validations
    }

    /**
     * Logs user into the system
     *
     * 
     *
     * This tests the overload of the method that uses a Map for query parameters instead of
     * listing them out individually.
     */
    @Test
    void loginUserTestQueryMap() {
        UserApi.LoginUserQueryParams queryParams = new UserApi.LoginUserQueryParams()
            .username(null)
            .password(null);
        // String response = api.loginUser(queryParams);

    // TODO: test validations
    }
    
    /**
     * Logs out current logged in user session
     *
     * 
     */
    @Test
    void logoutUserTest() {
        // api.logoutUser();

        // TODO: test validations
    }

    
    /**
     * Updated user
     *
     * This can only be done by the logged in user.
     */
    @Test
    void updateUserTest() {
        String username = null;
        User body = null;
        // api.updateUser(username, body);

        // TODO: test validations
    }

    
}
