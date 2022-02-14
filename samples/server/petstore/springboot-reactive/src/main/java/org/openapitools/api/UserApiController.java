package org.openapitools.api;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.beans.factory.annotation.Autowired;
import java.util.Optional;
import javax.annotation.Generated;

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
@Controller
@RequestMapping("${openapi.openAPIPetstore.base-path:/v2}")
public class UserApiController implements UserApi {

    private final UserApiDelegate delegate;

    public UserApiController(@Autowired(required = false) UserApiDelegate delegate) {
        this.delegate = Optional.ofNullable(delegate).orElse(new UserApiDelegate() {});
    }

    @Override
    public UserApiDelegate getDelegate() {
        return delegate;
    }

    /**
     * POST /user : Create user
     * This can only be done by the logged in user.
     *
     * @param body Created user object (required)
     * @return successful operation (status code 200)
     * @see UserApi#createUser
     */
    public Mono<ResponseEntity<Void>> createUser(
        @ApiParam(value = "Created user object", required = true) @Valid @RequestBody Mono<User> body
    ) {
        return delegate.createUser(body);
    }

    /**
     * POST /user/createWithArray : Creates list of users with given input array
     *
     * @param body List of user object (required)
     * @return successful operation (status code 200)
     * @see UserApi#createUsersWithArrayInput
     */
    public Mono<ResponseEntity<Void>> createUsersWithArrayInput(
        @ApiParam(value = "List of user object", required = true) @Valid @RequestBody Flux<User> body
    ) {
        return delegate.createUsersWithArrayInput(body);
    }

    /**
     * POST /user/createWithList : Creates list of users with given input array
     *
     * @param body List of user object (required)
     * @return successful operation (status code 200)
     * @see UserApi#createUsersWithListInput
     */
    public Mono<ResponseEntity<Void>> createUsersWithListInput(
        @ApiParam(value = "List of user object", required = true) @Valid @RequestBody Flux<User> body
    ) {
        return delegate.createUsersWithListInput(body);
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
    public Mono<ResponseEntity<Void>> deleteUser(
        @ApiParam(value = "The name that needs to be deleted", required = true) @PathVariable("username") String username
    ) {
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
    public Mono<ResponseEntity<User>> getUserByName(
        @ApiParam(value = "The name that needs to be fetched. Use user1 for testing.", required = true) @PathVariable("username") String username
    ) {
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
    public Mono<ResponseEntity<String>> loginUser(
        @NotNull @ApiParam(value = "The user name for login", required = true) @Valid @RequestParam(value = "username", required = true) String username,
        @NotNull @ApiParam(value = "The password for login in clear text", required = true) @Valid @RequestParam(value = "password", required = true) String password
    ) {
        return delegate.loginUser(username, password);
    }

    /**
     * GET /user/logout : Logs out current logged in user session
     *
     * @return successful operation (status code 200)
     * @see UserApi#logoutUser
     */
    public Mono<ResponseEntity<Void>> logoutUser(
        
    ) {
        return delegate.logoutUser();
    }

    /**
     * PUT /user/{username} : Updated user
     * This can only be done by the logged in user.
     *
     * @param username name that need to be deleted (required)
     * @param body Updated user object (required)
     * @return Invalid user supplied (status code 400)
     *         or User not found (status code 404)
     * @see UserApi#updateUser
     */
    public Mono<ResponseEntity<Void>> updateUser(
        @ApiParam(value = "name that need to be deleted", required = true) @PathVariable("username") String username,
        @ApiParam(value = "Updated user object", required = true) @Valid @RequestBody Mono<User> body
    ) {
        return delegate.updateUser(username, body);
    }

}
