package io.swagger.api;

import java.util.List;
import io.swagger.model.User;

import io.swagger.annotations.*;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.multipart.MultipartFile;
import java.io.IOException;

import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Api(value = "user", description = "the user API")
public interface UserApi {

    @ApiOperation(value = "Create user", notes = "This can only be done by the logged in user.", response = Void.class, tags={ "user", })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    
    @RequestMapping(value = "/user",
        produces = "application/json",
        consumes = "application/json",
        method = RequestMethod.POST)
    ResponseEntity<Void> createUser(@ApiParam(value = "Created user object" ,required=true )  @Valid @RequestBody User body);


    @ApiOperation(value = "Creates list of users with given input array", notes = "", response = Void.class, tags={ "user", })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    
    @RequestMapping(value = "/user/createWithArray",
        produces = "application/json",
        consumes = "application/json",
        method = RequestMethod.POST)
    ResponseEntity<Void> createUsersWithArrayInput(@ApiParam(value = "List of user object" ,required=true )  @Valid @RequestBody List<User> body);


    @ApiOperation(value = "Creates list of users with given input array", notes = "", response = Void.class, tags={ "user", })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    
    @RequestMapping(value = "/user/createWithList",
        produces = "application/json",
        consumes = "application/json",
        method = RequestMethod.POST)
    ResponseEntity<Void> createUsersWithListInput(@ApiParam(value = "List of user object" ,required=true )  @Valid @RequestBody List<User> body);


    @ApiOperation(value = "Delete user", notes = "This can only be done by the logged in user.", response = Void.class, tags={ "user", })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class) })
    
    @RequestMapping(value = "/user/{username}",
        produces = "application/json",
        consumes = "application/json",
        method = RequestMethod.DELETE)
    ResponseEntity<Void> deleteUser(@ApiParam(value = "The name that needs to be deleted",required=true ) @PathVariable("username") String username);


    @ApiOperation(value = "Get user by user name", notes = "", response = User.class, tags={ "user", })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = User.class),
        @ApiResponse(code = 400, message = "Invalid username supplied", response = User.class),
        @ApiResponse(code = 404, message = "User not found", response = User.class) })
    
    @RequestMapping(value = "/user/{username}",
        produces = "application/json",
        consumes = "application/json",
        method = RequestMethod.GET)
    ResponseEntity<User> getUserByName(@ApiParam(value = "The name that needs to be fetched. Use user1 for testing. ",required=true ) @PathVariable("username") String username) throws IOException;


    @ApiOperation(value = "Logs user into the system", notes = "", response = String.class, tags={ "user", })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = String.class),
        @ApiResponse(code = 400, message = "Invalid username/password supplied", response = String.class) })
    
    @RequestMapping(value = "/user/login",
        produces = "application/json",
        consumes = "application/json",
        method = RequestMethod.GET)
    ResponseEntity<String> loginUser( @NotNull @ApiParam(value = "The user name for login", required = true) @RequestParam(value = "username", required = true) String username, @NotNull @ApiParam(value = "The password for login in clear text", required = true) @RequestParam(value = "password", required = true) String password) throws IOException;


    @ApiOperation(value = "Logs out current logged in user session", notes = "", response = Void.class, tags={ "user", })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    
    @RequestMapping(value = "/user/logout",
        produces = "application/json",
        consumes = "application/json",
        method = RequestMethod.GET)
    ResponseEntity<Void> logoutUser();


    @ApiOperation(value = "Updated user", notes = "This can only be done by the logged in user.", response = Void.class, tags={ "user", })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid user supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class) })
    
    @RequestMapping(value = "/user/{username}",
        produces = "application/json",
        consumes = "application/json",
        method = RequestMethod.PUT)
    ResponseEntity<Void> updateUser(@ApiParam(value = "name that need to be deleted",required=true ) @PathVariable("username") String username,@ApiParam(value = "Updated user object" ,required=true )  @Valid @RequestBody User body);

}
