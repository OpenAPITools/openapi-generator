package org.openapitools.example.api.interfaces;

import java.time.OffsetDateTime;
import org.openapitools.example.model.User;
import org.openapitools.example.model.*;
import java.util.List;
import java.util.Map;
import java.time.OffsetDateTime;
import java.time.LocalDate;
import java.time.LocalDateTime;
import javax.annotation.Generated;


@Generated(value = "org.openapitools.codegen.languages.JavaDubboServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")

public interface UserService {

    /**
     * Create user
     * This can only be done by the logged in user.
     *
     * @param user Created user object (required)
     * @return void
     */
    void createUser(
        User user
    );

    /**
     * Creates list of users with given input array
     * 
     *
     * @param user List of user object (required)
     * @return void
     */
    void createUsersWithArrayInput(
        List<User> user
    );

    /**
     * Creates list of users with given input array
     * 
     *
     * @param user List of user object (required)
     * @return void
     */
    void createUsersWithListInput(
        List<User> user
    );

    /**
     * Delete user
     * This can only be done by the logged in user.
     *
     * @param username The name that needs to be deleted (required)
     * @return void
     */
    void deleteUser(
        String username
    );

    /**
     * Get user by user name
     * 
     *
     * @param username The name that needs to be fetched. Use user1 for testing. (required)
     * @return User
     */
    User getUserByName(
        String username
    );

    /**
     * Logs user into the system
     * 
     *
     * @param username The user name for login (required)
     * @param password The password for login in clear text (required)
     * @return String
     */
    String loginUser(
        String username,
        String password
    );

    /**
     * Logs out current logged in user session
     * 
     *
     * @return void
     */
    void logoutUser(
    );

    /**
     * Updated user
     * This can only be done by the logged in user.
     *
     * @param username name that need to be deleted (required)
     * @param user Updated user object (required)
     * @return void
     */
    void updateUser(
        String username,
        User user
    );
}
