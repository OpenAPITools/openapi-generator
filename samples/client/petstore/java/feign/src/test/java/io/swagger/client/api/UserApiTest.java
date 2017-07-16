package io.swagger.client.api;

import io.swagger.TestUtils;

import io.swagger.client.ApiClient;
import io.swagger.client.model.*;

import java.util.Arrays;

import org.junit.*;
import static org.junit.Assert.*;

public class UserApiTest {
    private UserApi api;

    @Before
    public void setup() {
        ApiClient apiClient = new ApiClient();
        api = apiClient.buildClient(UserApi.class);
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
        user1.setUsername("user" + user1.getId());
        User user2 = createUser();
        user2.setUsername("user" + user2.getId());

        api.createUsersWithArrayInput(Arrays.asList(user1, user2));

        User fetched = api.getUserByName(user1.getUsername());
        assertEquals(user1.getId(), fetched.getId());
    }

    @Test
    public void testCreateUsersWithList() throws Exception {
        User user1 = createUser();
        user1.setUsername("user" + user1.getId());
        User user2 = createUser();
        user2.setUsername("user" + user2.getId());

        api.createUsersWithListInput(Arrays.asList(user1, user2));

        User fetched = api.getUserByName(user1.getUsername());
        assertEquals(user1.getId(), fetched.getId());
    }

    // ignore for the time being, please refer to the following for more info:
    // https://github.com/swagger-api/swagger-codegen/issues/1660
    @Ignore @Test
    public void testLoginUser() throws Exception {
        User user = createUser();
        api.createUser(user);

        String token = api.loginUser(user.getUsername(), user.getPassword());
        assertTrue(token.startsWith("logged in user session:"));

        UserApi.LoginUserQueryParams queryParams = new UserApi.LoginUserQueryParams()
                .username(user.getUsername())
                .password(user.getPassword());
        token = api.loginUser(queryParams);
        assertTrue(token.startsWith("logged in user session:"));
    }

    @Test
    public void logoutUser() throws Exception {
        api.logoutUser();
    }

    private User createUser() {
        User user = new User();
        user.setId(TestUtils.nextId());
        user.setUsername("fred" + user.getId());
        user.setFirstName("Fred");
        user.setLastName("Meyer");
        user.setEmail("fred@fredmeyer.com");
        user.setPassword("xxXXxx");
        user.setPhone("408-867-5309");
        user.setUserStatus(123);

        return user;
    }
}
