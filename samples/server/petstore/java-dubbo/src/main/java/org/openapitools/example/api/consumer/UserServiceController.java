package org.openapitools.example.api.consumer;

import java.time.OffsetDateTime;
import org.openapitools.example.model.User;
import org.openapitools.example.model.*;
import org.openapitools.example.api.interfaces.UserService;
import java.util.List;
import java.util.Map;
import java.time.OffsetDateTime;
import java.time.LocalDate;
import java.time.LocalDateTime;
import org.apache.dubbo.config.annotation.DubboReference;
import org.springframework.web.bind.annotation.*;
import javax.annotation.Generated;


@Generated(value = "org.openapitools.codegen.languages.JavaDubboServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")

@RestController
@RequestMapping("/user")
public class UserServiceController {

    @DubboReference
    private UserService userService;

    @RequestMapping(method = RequestMethod.POST, value = "/")
    public void createUser(
        @RequestParam(name = "user") User user
    ) {
        userService.createUser(user);
    }

    @RequestMapping(method = RequestMethod.POST, value = "/createWithArray")
    public void createUsersWithArrayInput(
        @RequestParam(name = "user") List<User> user
    ) {
        userService.createUsersWithArrayInput(user);
    }

    @RequestMapping(method = RequestMethod.POST, value = "/createWithList")
    public void createUsersWithListInput(
        @RequestParam(name = "user") List<User> user
    ) {
        userService.createUsersWithListInput(user);
    }

    @RequestMapping(method = RequestMethod.DELETE, value = "/{username}")
    public void deleteUser(
        @RequestParam(name = "username") String username
    ) {
        userService.deleteUser(username);
    }

    @RequestMapping(method = RequestMethod.GET, value = "/{username}")
    public User getUserByName(
        @RequestParam(name = "username") String username
    ) {
        return userService.getUserByName(username);
    }

    @RequestMapping(method = RequestMethod.GET, value = "/login")
    public String loginUser(
        @RequestParam(name = "username") String username,
        @RequestParam(name = "password") String password
    ) {
        return userService.loginUser(username, password);
    }

    @RequestMapping(method = RequestMethod.GET, value = "/logout")
    public void logoutUser(
    ) {
        userService.logoutUser();
    }

    @RequestMapping(method = RequestMethod.PUT, value = "/{username}")
    public void updateUser(
        @RequestParam(name = "username") String username,
        @RequestParam(name = "user") User user
    ) {
        userService.updateUser(username, user);
    }
}
