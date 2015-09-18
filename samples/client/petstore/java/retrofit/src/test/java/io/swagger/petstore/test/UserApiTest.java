package io.swagger.petstore.test;

import io.swagger.client.ApiClient;
import io.swagger.client.api.*;
import io.swagger.client.model.*;


import java.util.Arrays;

import org.junit.*;
import static org.junit.Assert.*;

public class UserApiTest {
    UserApi api = null;

    @Before
    public void setup() {
        api = new ApiClient().createService(UserApi.class);
    }

    @Test
    public void testCreateUser() throws Exception {
        User user = createUser();

        api.createUser(user);

        User fetched = api.getUserByName(user.getUsername());
        assertEquals(user.getId(), fetched.getId());
    }

    @Test
    public void testCreateUsersWithArray() throws Exception {
        User user1 = createUser();
        user1.setUsername("abc123");
        User user2 = createUser();
        user2.setUsername("123abc");

        api.createUsersWithArrayInput(Arrays.asList(new User[]{user1, user2}));

        User fetched = api.getUserByName(user1.getUsername());
        assertEquals(user1.getId(), fetched.getId());
    }

    @Test
    public void testCreateUsersWithList() throws Exception {
        User user1 = createUser();
        user1.setUsername("abc123");
        User user2 = createUser();
        user2.setUsername("123abc");

        api.createUsersWithListInput(Arrays.asList(new User[]{user1, user2}));

        User fetched = api.getUserByName(user1.getUsername());
        assertEquals(user1.getId(), fetched.getId());
    }

    @Test
    public void testLoginUser() throws Exception {
        User user = createUser();
        api.createUser(user);

        String token = api.loginUser(user.getUsername(), user.getPassword());
        assertTrue(token.startsWith("logged in user session:"));
    }

    @Test
    public void logoutUser() throws Exception {
        api.logoutUser();
    }

    private User createUser() {
        User user = new User();
        user.setId(System.currentTimeMillis());
        user.setUsername("fred");
        user.setFirstName("Fred");
        user.setLastName("Meyer");
        user.setEmail("fred@fredmeyer.com");
        user.setPassword("xxXXxx");
        user.setPhone("408-867-5309");
        user.setUserStatus(123);

        return user;
    }
}