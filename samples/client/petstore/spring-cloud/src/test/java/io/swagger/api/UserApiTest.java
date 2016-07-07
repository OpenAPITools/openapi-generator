package io.swagger.api;

import io.swagger.TestUtils;
import io.swagger.model.User;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.cloud.netflix.feign.EnableFeignClients;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.Arrays;

import static org.junit.Assert.*;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(classes = UserApiTest.Application.class)

public class UserApiTest {

    @Autowired
    private UserApiClient client;

    @Test
    public void testCreateUser() {
        User user = createUser();

        client.createUser(user);

        User fetched = client.getUserByName(user.getUsername()).getBody();
        assertEquals(user.getId(), fetched.getId());
    }

    @Test
    public void testCreateUsersWithArray() {
        User user1 = createUser();
        user1.setUsername("user" + user1.getId());
        User user2 = createUser();
        user2.setUsername("user" + user2.getId());

        client.createUsersWithArrayInput(Arrays.asList(new User[]{user1, user2}));

        User fetched = client.getUserByName(user1.getUsername()).getBody();
        assertEquals(user1.getId(), fetched.getId());
    }

    @Test
    public void testCreateUsersWithList() {
        User user1 = createUser();
        user1.setUsername("user" + user1.getId());
        User user2 = createUser();
        user2.setUsername("user" + user2.getId());

        client.createUsersWithListInput(Arrays.asList(new User[]{user1, user2}));

        User fetched = client.getUserByName(user1.getUsername()).getBody();
        assertEquals(user1.getId(), fetched.getId());
    }

    @Test
    public void testLoginUser() {
        User user = createUser();
        client.createUser(user);

        String token = client.loginUser(user.getUsername(), user.getPassword()).getBody();
        assertTrue(token.startsWith("logged in user session:"));
    }

    @Test
    public void logoutUser() {
        client.logoutUser();
    }

    private User createUser() {
        User user = new User();
        user.setId(TestUtils.nextId());
        user.setUsername("fred");
        user.setFirstName("Fred");
        user.setLastName("Meyer");
        user.setEmail("fred@fredmeyer.com");
        user.setPassword("xxXXxx");
        user.setPhone("408-867-5309");
        user.setUserStatus(123);

        return user;
    }

    @SpringBootApplication
    @EnableFeignClients
    protected static class Application {
        public static void main(String[] args) {
            new SpringApplicationBuilder(UserApiTest.Application.class).run(args);
        }
    }
}
