package org.openapitools.api;

import java.util.List;
import org.openapitools.model.User;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.Explode;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.enums.ParameterStyle;
import io.swagger.v3.oas.annotations.enums.SecuritySchemeType;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.OAuthFlow;
import io.swagger.v3.oas.annotations.security.OAuthFlows;
import io.swagger.v3.oas.annotations.security.OAuthScope;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.security.SecurityScheme;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.constraints.*;
import javax.validation.Valid;
import java.util.List;
import java.util.Map;

@Controller
@RequestMapping("${openapi.openAPIPetstore.base-path:/v2}")
public class UserApiController implements UserApi {

    private final UserApiDelegate delegate;

    public UserApiController(@org.springframework.beans.factory.annotation.Autowired(required = false) UserApiDelegate delegate) {
        this.delegate = delegate;
    }

    /**
     * POST /user : Create user
     * This can only be done by the logged in user.
     *
     * @param user Created user object (required)
     * @return successful operation (status code 200)
     * @see UserApi#createUser
     */
    public ResponseEntity<Void> createUser(@Parameter(description = "Created user object" ,required=true )  @Valid @RequestBody User user) {
        return delegate.createUser(user);
    }

    /**
     * POST /user/createWithArray : Creates list of users with given input array
     *
     * @param user List of user object (required)
     * @return successful operation (status code 200)
     * @see UserApi#createUsersWithArrayInput
     */
    public ResponseEntity<Void> createUsersWithArrayInput(@Parameter(description = "List of user object" ,required=true )  @Valid @RequestBody List<User> user) {
        return delegate.createUsersWithArrayInput(user);
    }

    /**
     * POST /user/createWithList : Creates list of users with given input array
     *
     * @param user List of user object (required)
     * @return successful operation (status code 200)
     * @see UserApi#createUsersWithListInput
     */
    public ResponseEntity<Void> createUsersWithListInput(@Parameter(description = "List of user object" ,required=true )  @Valid @RequestBody List<User> user) {
        return delegate.createUsersWithListInput(user);
    }

    /**
     * DELETE /user/{username} : Delete user
     * This can only be done by the logged in user.
     *
     * @param username The name that needs to be deleted (required)
     * @return Invalid username supplied (status code 400)
     *         or User not found (status code 404)
     * @see UserApi#deleteUser
     */
    public ResponseEntity<Void> deleteUser(@Parameter(description = "The name that needs to be deleted",required=true) @PathVariable("username") String username) {
        return delegate.deleteUser(username);
    }

    /**
     * GET /user/{username} : Get user by user name
     *
     * @param username The name that needs to be fetched. Use user1 for testing. (required)
     * @return successful operation (status code 200)
     *         or Invalid username supplied (status code 400)
     *         or User not found (status code 404)
     * @see UserApi#getUserByName
     */
    public ResponseEntity<User> getUserByName(@Parameter(description = "The name that needs to be fetched. Use user1 for testing.",required=true) @PathVariable("username") String username) {
        return delegate.getUserByName(username);
    }

    /**
     * GET /user/login : Logs user into the system
     *
     * @param username The user name for login (required)
     * @param password The password for login in clear text (required)
     * @return successful operation (status code 200)
     *         or Invalid username/password supplied (status code 400)
     * @see UserApi#loginUser
     */
    public ResponseEntity<String> loginUser(@NotNull @Pattern(regexp="^[a-zA-Z0-9]+[a-zA-Z0-9\\.\\-_]*[a-zA-Z0-9]+$") @Parameter(description = "The user name for login", required = true) @Valid @RequestParam(value = "username", required = true) String username,@NotNull @Parameter(description = "The password for login in clear text", required = true) @Valid @RequestParam(value = "password", required = true) String password) {
        return delegate.loginUser(username, password);
    }

    /**
     * GET /user/logout : Logs out current logged in user session
     *
     * @return successful operation (status code 200)
     * @see UserApi#logoutUser
     */
    public ResponseEntity<Void> logoutUser() {
        return delegate.logoutUser();
    }

    /**
     * PUT /user/{username} : Updated user
     * This can only be done by the logged in user.
     *
     * @param username name that need to be deleted (required)
     * @param user Updated user object (required)
     * @return Invalid user supplied (status code 400)
     *         or User not found (status code 404)
     * @see UserApi#updateUser
     */
    public ResponseEntity<Void> updateUser(@Parameter(description = "name that need to be deleted",required=true) @PathVariable("username") String username,@Parameter(description = "Updated user object" ,required=true )  @Valid @RequestBody User user) {
        return delegate.updateUser(username, user);
    }

}
