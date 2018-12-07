package org.openapitools.client.api;

import java.util.List;
import java.util.function.Consumer;

import org.openapitools.client.model.*;
import net.java.html.json.Model;
import net.java.html.json.ModelOperation;
import net.java.html.json.OnReceive;
import net.java.html.json.Property;

@ModelOperation
@Model(className = "UserApi", targetId = "", properties = {
    @Property(name="url", type=String.class )
})
public class UserApiConnector {
    // Create user
    @OnReceive(method = "POST", data=User.class,  url = "{url}/user")
    public static void createUser( UserApi model, User user, Consumer<Throwable> onError) {

    }

    // Creates list of users with given input array
    @OnReceive(method = "POST", data=List.class,  url = "{url}/user/createWithArray")
    public static void createUsersWithArrayInput( UserApi model, List user, Consumer<Throwable> onError) {

    }

    // Creates list of users with given input array
    @OnReceive(method = "POST", data=List.class,  url = "{url}/user/createWithList")
    public static void createUsersWithListInput( UserApi model, List user, Consumer<Throwable> onError) {

    }

    // Delete user
    @OnReceive(method = "DELETE",  url = "{url}/user/{username}")
    public static void deleteUser( UserApi model, Consumer<Throwable> onError) {

    }

    // Get user by user name
    @OnReceive(method = "GET",  url = "{url}/user/{username}")
    public static void getUserByName( UserApi model, User result,Consumer< User> onSuccess, Consumer<Throwable> onError) {

    }

    // Logs user into the system
    @OnReceive(method = "GET",  url = "{url}/user/login")
    public static void loginUser( UserApi model, User result,Consumer< User> onSuccess, Consumer<Throwable> onError) {

    }

    // Logs out current logged in user session
    @OnReceive(method = "GET",  url = "{url}/user/logout")
    public static void logoutUser( UserApi model, Consumer<Throwable> onError) {

    }

    // Updated user
    @OnReceive(method = "PUT", data=User.class,  url = "{url}/user/{username}")
    public static void updateUser( UserApi model, User user, Consumer<Throwable> onError) {

    }

}
