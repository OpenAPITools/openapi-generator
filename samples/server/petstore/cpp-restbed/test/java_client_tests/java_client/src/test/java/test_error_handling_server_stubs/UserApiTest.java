package test_error_handling_server_stubs;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openapi.example.api.UserApi;
import org.openapi.example.model.User;
import org.springframework.web.reactive.function.client.WebClientResponseException;

import java.util.ArrayList;
import java.util.List;

import static helper.ApiClientFactories.setUpUserApi;
import static helper.TestingHelper.approveException;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class UserApiTest {

    UserApi apiInstance;

    @BeforeEach
    void setUp() {
        apiInstance = setUpUserApi(1236);
    }

    @Test
    void createUserThrowsApiException() {
        User user = new User();
        user.setFirstName("ThrowsApiException");
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.createUser(user).block();
                });
        approveException(exception);
    }

    @Test
    void createUserThrowsStdExceptionDerivedException() {
        User user = new User();
        user.setFirstName("ThrowsStdExceptionDerivedException");
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.createUser(user).block();
                });
        approveException(exception);
    }

    @Test
    void createUserThrowsInt() {
        User user = new User();
        user.setFirstName("ThrowsInt");
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.createUser(user).block();
                });
        approveException(exception);
    }

    @Test
    void createUserReturnsStatus0() {
        User user = new User();
        user.setFirstName("ReturnsStatus0");

        apiInstance.createUser(user).block();
    }


    @Test
    void createUserReturnsStatus400() {
        User user = new User();
        user.setFirstName("ReturnsStatus400");
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.createUser(user).block();
                });
        approveException(exception);
    }

    @Test
    void createUserReturnsStatus500() {
        User user = new User();
        user.setFirstName("ReturnsStatus500");
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.createUser(user).block();
                });
        approveException(exception);
    }

    @Test
    void createUsersWithArrayInputThrowsApiException() {
        List<User> users = new ArrayList<>();
        User user = new User();
        user.setFirstName("ThrowsApiException");
        users.add(user);
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.createUsersWithArrayInput(users).block();
                });
        approveException(exception);
    }

    @Test
    void createUsersWithArrayInputThrowsStdExceptionDerivedException() {
        List<User> users = new ArrayList<>();
        User user = new User();
        user.setFirstName("ThrowsStdExceptionDerivedException");
        users.add(user);
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.createUsersWithArrayInput(users).block();
                });
        approveException(exception);
    }

    @Test
    void createUsersWithArrayInputThrowsInt() {
        List<User> users = new ArrayList<>();
        User user = new User();
        user.setFirstName("ThrowsInt");
        users.add(user);
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.createUsersWithArrayInput(users).block();
                });
        approveException(exception);
    }

    @Test
    void createUsersWithArrayInputReturnsStatus0() {
        List<User> users = new ArrayList<>();
        User user = new User();
        user.setFirstName("ReturnsStatus0");
        users.add(user);

        apiInstance.createUsersWithArrayInput(users).block();
    }

    @Test
    void createUsersWithArrayInputReturnsStatus400() {
        List<User> users = new ArrayList<>();
        User user = new User();
        user.setFirstName("ReturnsStatus400");
        users.add(user);

        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.createUsersWithArrayInput(users).block();
                });
        approveException(exception);
    }

    @Test
    void createUsersWithArrayInputReturnsStatus500() {
        List<User> users = new ArrayList<>();
        User user = new User();
        user.setFirstName("ReturnsStatus500");
        users.add(user);

        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.createUsersWithArrayInput(users).block();
                });
        approveException(exception);
    }

    @Test
    void createUsersWithListInputThrowsApiException() {
        List<User> users = new ArrayList<>();
        User user = new User();
        user.setFirstName("ThrowsApiException");
        users.add(user);
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.createUsersWithListInput(users).block();
                });
        approveException(exception);
    }

    @Test
    void createUsersWithListInputThrowsStdExceptionDerivedException() {
        List<User> users = new ArrayList<>();
        User user = new User();
        user.setFirstName("ThrowsStdExceptionDerivedException");
        users.add(user);
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.createUsersWithListInput(users).block();
                });
        approveException(exception);
    }

    @Test
    void createUsersWithListInputThrowsInt() {
        List<User> users = new ArrayList<>();
        User user = new User();
        user.setFirstName("ThrowsInt");
        users.add(user);
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.createUsersWithListInput(users).block();
                });
        approveException(exception);
    }

    @Test
    void createUsersWithListInputReturnsStatus0() {
        List<User> users = new ArrayList<>();
        User user = new User();
        user.setFirstName("ReturnsStatus0");
        users.add(user);

        apiInstance.createUsersWithListInput(users).block();
    }

    @Test
    void createUsersWithListInputReturnsStatus400() {
        List<User> users = new ArrayList<>();
        User user = new User();
        user.setFirstName("ReturnsStatus400");
        users.add(user);

        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.createUsersWithListInput(users).block();
                });
        approveException(exception);
    }

    @Test
    void createUsersWithListInputReturnsStatus500() {
        List<User> users = new ArrayList<>();
        User user = new User();
        user.setFirstName("ReturnsStatus500");
        users.add(user);

        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.createUsersWithListInput(users).block();
                });
        approveException(exception);
    }

    @Test
    void loginUserThrowsApiException() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.loginUser("ThrowsApiException", "mypassword").block();
                });
        approveException(exception);
    }

    @Test
    void loginUserThrowsStdExceptionDerivedException() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.loginUser("ThrowsStdExceptionDerivedException", "mypassword").block();
                });
        approveException(exception);
    }

    @Test
    void loginUserThrowsInt() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.loginUser("ThrowsInt", "mypassword").block();
                });
        approveException(exception);
    }

    @Test
    void loginUserReturnsStatus200() {
        String resp = apiInstance.loginUser("ReturnsStatus200", "mypassword").block();
        assertEquals("ReturnsStatus200", resp);
    }

    @Test
    void loginUserReturnsStatus400() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.loginUser("ReturnsStatus400", "mypassword").block();
                });
        approveException(exception);
    }

    @Test
    void loginUserReturnsStatus500() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.loginUser("ReturnsStatus500", "mypassword").block();
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