package test_default_unimplemented_server_stubs;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openapi.example.api.UserApi;
import org.openapi.example.model.User;
import org.springframework.web.reactive.function.client.WebClientResponseException;

import java.util.Arrays;

import static helper.ApiClientFactories.setUpUserApi;
import static org.junit.jupiter.api.Assertions.*;
import static helper.TestingHelper.approveException;

class UserApiTest {

    UserApi apiInstance;

    @BeforeEach
    void setUp() {
        apiInstance = setUpUserApi(1234);
    }

    @Test
    void createUser() {
        User user = new User();
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.createUser(user).block();
        });

        approveException(exception);
    }

    @Test
    void createUsersWithArrayInput() {
        User user = new User();
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.createUsersWithArrayInput(Arrays.asList(user)).block();
                });

        approveException(exception);
    }

    @Test
    void createUsersWithListInput() {
        User user = new User();
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.createUsersWithListInput(Arrays.asList(user)).block();
                });
        approveException(exception);
    }

    @Test
    void loginUser() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.loginUser("my name", "my password").block();
                });

        approveException(exception);
    }

    @Test
    void logoutUser() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.logoutUser().block();
                });

        approveException(exception);
    }
}