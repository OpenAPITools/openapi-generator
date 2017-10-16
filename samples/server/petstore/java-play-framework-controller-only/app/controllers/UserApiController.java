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
import java.io.File;
import swagger.SwaggerUtils;
import com.fasterxml.jackson.core.type.TypeReference;

import javax.validation.constraints.*;

import swagger.SwaggerUtils.ApiAction;


public class UserApiController extends Controller {

    private final ObjectMapper mapper;

    @Inject
    private UserApiController() {
        mapper = new ObjectMapper();
    }


    @ApiAction
    public Result createUser() throws Exception {
        JsonNode nodebody = request().body().asJson();
        User body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), User.class);
            body.validate();
        } else {
            throw new IllegalArgumentException("'body' parameter is required");
        }
        return ok();
    }

    @ApiAction
    public Result createUsersWithArrayInput() throws Exception {
        JsonNode nodebody = request().body().asJson();
        List<User> body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), new TypeReference<List<User>>(){});
            for (User curItem : body) {
                curItem.validate();
            }
        } else {
            throw new IllegalArgumentException("'body' parameter is required");
        }
        return ok();
    }

    @ApiAction
    public Result createUsersWithListInput() throws Exception {
        JsonNode nodebody = request().body().asJson();
        List<User> body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), new TypeReference<List<User>>(){});
            for (User curItem : body) {
                curItem.validate();
            }
        } else {
            throw new IllegalArgumentException("'body' parameter is required");
        }
        return ok();
    }

    @ApiAction
    public Result deleteUser(String username) throws Exception {
        return ok();
    }

    @ApiAction
    public Result getUserByName(String username) throws Exception {
        return ok();
    }

    @ApiAction
    public Result loginUser() throws Exception {
        String valueusername = request().getQueryString("username");
        String username;
        if (valueusername != null) {
            username = valueusername;
        } else {
            throw new IllegalArgumentException("'username' parameter is required");
        }
        String valuepassword = request().getQueryString("password");
        String password;
        if (valuepassword != null) {
            password = valuepassword;
        } else {
            throw new IllegalArgumentException("'password' parameter is required");
        }
        return ok();
    }

    @ApiAction
    public Result logoutUser() throws Exception {
        return ok();
    }

    @ApiAction
    public Result updateUser(String username) throws Exception {
        JsonNode nodebody = request().body().asJson();
        User body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), User.class);
            body.validate();
        } else {
            throw new IllegalArgumentException("'body' parameter is required");
        }
        return ok();
    }
}
