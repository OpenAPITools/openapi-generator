package io.swagger.api;

import java.util.List;
import io.swagger.model.User;

import io.swagger.annotations.*;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

import javax.validation.constraints.*;

@Controller
public class UserApiController implements UserApi {
    private final UserApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public UserApiController(UserApiDelegate delegate) {
        this.delegate = delegate;
    }


    public ResponseEntity<Void> createUser(@ApiParam(value = "Created user object" ,required=true ) @RequestBody User body) {
        // do some magic!
        return delegate.createUser(body);
    }

    public ResponseEntity<Void> createUsersWithArrayInput(@ApiParam(value = "List of user object" ,required=true ) @RequestBody List<User> body) {
        // do some magic!
        return delegate.createUsersWithArrayInput(body);
    }

    public ResponseEntity<Void> createUsersWithListInput(@ApiParam(value = "List of user object" ,required=true ) @RequestBody List<User> body) {
        // do some magic!
        return delegate.createUsersWithListInput(body);
    }

    public ResponseEntity<Void> deleteUser(@ApiParam(value = "The name that needs to be deleted",required=true ) @PathVariable("username") String username) {
        // do some magic!
        return delegate.deleteUser(username);
    }

    public ResponseEntity<User> getUserByName(@ApiParam(value = "The name that needs to be fetched. Use user1 for testing. ",required=true ) @PathVariable("username") String username) {
        // do some magic!
        return delegate.getUserByName(username);
    }

    public ResponseEntity<String> loginUser( @NotNull @ApiParam(value = "The user name for login", required = true) @RequestParam(value = "username", required = true) String username,
         @NotNull @ApiParam(value = "The password for login in clear text", required = true) @RequestParam(value = "password", required = true) String password) {
        // do some magic!
        return delegate.loginUser(username, password);
    }

    public ResponseEntity<Void> logoutUser() {
        // do some magic!
        return delegate.logoutUser();
    }

    public ResponseEntity<Void> updateUser(@ApiParam(value = "name that need to be deleted",required=true ) @PathVariable("username") String username,
        @ApiParam(value = "Updated user object" ,required=true ) @RequestBody User body) {
        // do some magic!
        return delegate.updateUser(username, body);
    }

}
