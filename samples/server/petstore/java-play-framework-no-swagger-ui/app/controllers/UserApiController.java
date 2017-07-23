package controllers;

import java.util.List;
import apimodels.User;

import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Http;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.google.inject.Inject;
import java.io.IOException;
import swagger.SwaggerUtils;
import com.fasterxml.jackson.core.type.TypeReference;

import javax.validation.constraints.*;

import swagger.SwaggerUtils.ApiAction;


public class UserApiController extends Controller {

    private final UserApiControllerImp imp;
    private final ObjectMapper mapper;

    @Inject
    private UserApiController(UserApiControllerImp imp) {
        this.imp = imp;
        mapper = new ObjectMapper();
    }


    @ApiAction
    public Result createUser() throws Exception {
        JsonNode nodebody = request().body().asJson();
        User body;

        body = mapper.readValue(nodebody.toString(), User.class);
        body.validate();

        imp.createUser(body);
        
        return ok();
        
    }

    @ApiAction
    public Result createUsersWithArrayInput() throws Exception {
        JsonNode nodebody = request().body().asJson();
        List<User> body;

        body = mapper.readValue(nodebody.toString(), new TypeReference<List<User>>(){});
        for (User curItem : body) {
            curItem.validate();
        }

        imp.createUsersWithArrayInput(body);
        
        return ok();
        
    }

    @ApiAction
    public Result createUsersWithListInput() throws Exception {
        JsonNode nodebody = request().body().asJson();
        List<User> body;

        body = mapper.readValue(nodebody.toString(), new TypeReference<List<User>>(){});
        for (User curItem : body) {
            curItem.validate();
        }

        imp.createUsersWithListInput(body);
        
        return ok();
        
    }

    @ApiAction
    public Result deleteUser(String username) throws Exception {
        imp.deleteUser(username);
        
        return ok();
        
    }

    @ApiAction
    public Result getUserByName(String username) throws Exception {
        User obj = imp.getUserByName(username);
        obj.validate();
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
        
    }

    @ApiAction
    public Result loginUser() throws Exception {
        String valueusername = request().getQueryString("username");
        String username;

        username = (String)valueusername;

        String valuepassword = request().getQueryString("password");
        String password;

        password = (String)valuepassword;

        String obj = imp.loginUser(username, password);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
        
    }

    @ApiAction
    public Result logoutUser() throws Exception {
        imp.logoutUser();
        
        return ok();
        
    }

    @ApiAction
    public Result updateUser(String username) throws Exception {
        JsonNode nodebody = request().body().asJson();
        User body;

        body = mapper.readValue(nodebody.toString(), User.class);
        body.validate();

        imp.updateUser(username, body);
        
        return ok();
        
    }
}
