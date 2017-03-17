package controllers;

import java.util.List;
import apimodels.User;

import io.swagger.annotations.*;
import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.google.inject.Inject;
import java.io.IOException;
import swagger.SwaggerUtils;
import javafx.util.Pair;
import com.fasterxml.jackson.core.type.TypeReference;

import javax.validation.constraints.*;


@Api(value = "User", description = "the User API")
public class UserApiController extends Controller {

    private UserApiControllerImp imp;
    private ObjectMapper mapper;

    @Inject
    private UserApiController(UserApiControllerImp imp) {
        this.imp = imp;
        mapper = new ObjectMapper();
    }


    @ApiOperation(value = "Create user", notes = "This can only be done by the logged in user.", tags={  })
    @ApiResponses(value = { 
    @ApiResponse(code = 0, message = "successful operation") })
    @ApiImplicitParams({
        @ApiImplicitParam(name = "body", value = "Created user object", dataType = "apimodels.User", paramType = "body")
    })
    public Result createUser() throws IOException {
        JsonNode nodebody = request().body().asJson();
        User body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), User.class);
        
        } else {
            body = null;
        }
        imp.createUser(body);
        
        return ok();
    }

    @ApiOperation(value = "Creates list of users with given input array", notes = "", tags={  })
    @ApiResponses(value = { 
    @ApiResponse(code = 0, message = "successful operation") })
    @ApiImplicitParams({
        @ApiImplicitParam(name = "body", value = "List of user object", dataType = "List<User>", paramType = "body")
    })
    public Result createUsersWithArrayInput() throws IOException {
        JsonNode nodebody = request().body().asJson();
        List<User> body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), new TypeReference<List<List<User>>>(){});
        
        } else {
            body = null;
        }
        imp.createUsersWithArrayInput(body);
        
        return ok();
    }

    @ApiOperation(value = "Creates list of users with given input array", notes = "", tags={  })
    @ApiResponses(value = { 
    @ApiResponse(code = 0, message = "successful operation") })
    @ApiImplicitParams({
        @ApiImplicitParam(name = "body", value = "List of user object", dataType = "List<User>", paramType = "body")
    })
    public Result createUsersWithListInput() throws IOException {
        JsonNode nodebody = request().body().asJson();
        List<User> body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), new TypeReference<List<List<User>>>(){});
        
        } else {
            body = null;
        }
        imp.createUsersWithListInput(body);
        
        return ok();
    }

    @ApiOperation(value = "Delete user", notes = "This can only be done by the logged in user.", tags={  })
    @ApiResponses(value = { 
    @ApiResponse(code = 400, message = "Invalid username supplied"), 
    @ApiResponse(code = 404, message = "User not found") })
    @ApiImplicitParams({
        
    })
    public Result deleteUser(@ApiParam(value = "The name that needs to be deleted", required = true ) String username)  {
        imp.deleteUser(username);
        
        return ok();
    }

    @ApiOperation(value = "Get user by user name", notes = "", response = User.class, tags={  })
    @ApiResponses(value = { 
    @ApiResponse(code = 200, message = "successful operation", response = User.class), 
    @ApiResponse(code = 400, message = "Invalid username supplied", response = User.class), 
    @ApiResponse(code = 404, message = "User not found", response = User.class) })
    @ApiImplicitParams({
        
    })
    public Result getUserByName(@ApiParam(value = "The name that needs to be fetched. Use user1 for testing. ", required = true ) String username)  {
        User obj = imp.getUserByName(username);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
    }

    @ApiOperation(value = "Logs user into the system", notes = "", response = String.class, tags={  })
    @ApiResponses(value = { 
    @ApiResponse(code = 200, message = "successful operation", response = String.class), 
    @ApiResponse(code = 400, message = "Invalid username/password supplied", response = String.class) })
    @ApiImplicitParams({
        @ApiImplicitParam(name = "username", value = "The user name for login", dataType = "String", paramType = "query"),
        @ApiImplicitParam(name = "password", value = "The password for login in clear text", dataType = "String", paramType = "query")
    })
    public Result loginUser()  {
        String valueusername = request().getQueryString("username");
        String username;
        if (valueusername != null) {
            username = (String)valueusername;
        
        } else {
            username = "";
        }
        String valuepassword = request().getQueryString("password");
        String password;
        if (valuepassword != null) {
            password = (String)valuepassword;
        
        } else {
            password = "";
        }
        String obj = imp.loginUser(username, password);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
    }

    @ApiOperation(value = "Logs out current logged in user session", notes = "", tags={  })
    @ApiResponses(value = { 
    @ApiResponse(code = 0, message = "successful operation") })
    public Result logoutUser()  {
        imp.logoutUser();
        
        return ok();
    }

    @ApiOperation(value = "Updated user", notes = "This can only be done by the logged in user.", tags={  })
    @ApiResponses(value = { 
    @ApiResponse(code = 400, message = "Invalid user supplied"), 
    @ApiResponse(code = 404, message = "User not found") })
    @ApiImplicitParams({
        @ApiImplicitParam(name = "body", value = "Updated user object", dataType = "apimodels.User", paramType = "body")
    })
    public Result updateUser(@ApiParam(value = "name that need to be deleted", required = true ) String username) throws IOException {
        JsonNode nodebody = request().body().asJson();
        User body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), User.class);
        
        } else {
            body = null;
        }
        imp.updateUser(username, body);
        
        return ok();
    }
}
