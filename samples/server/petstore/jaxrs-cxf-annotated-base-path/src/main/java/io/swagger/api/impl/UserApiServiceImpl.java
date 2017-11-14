package io.swagger.api.impl;

import io.swagger.api.*;
import java.util.List;
import io.swagger.model.User;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import org.apache.cxf.jaxrs.model.wadl.Description;
import org.apache.cxf.jaxrs.model.wadl.DocTarget;

import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.annotations.Api;

/**
 * Swagger Petstore
 *
 * <p>This is a sample server Petstore server.  You can find out more about Swagger at [http://swagger.io](http://swagger.io) or on [irc.freenode.net, #swagger](http://swagger.io/irc/).  For this sample, you can use the api key `special-key` to test the authorization filters.
 *
 */
public class UserApiServiceImpl implements UserApi {
    /**
     * Create user
     *
     * This can only be done by the logged in user.
     *
     */
    public void createUser(User body) {
        // TODO: Implement...
        
        
    }
    
    /**
     * Creates list of users with given input array
     *
     * 
     *
     */
    public void createUsersWithArrayInput(List<User> body) {
        // TODO: Implement...
        
        
    }
    
    /**
     * Creates list of users with given input array
     *
     * 
     *
     */
    public void createUsersWithListInput(List<User> body) {
        // TODO: Implement...
        
        
    }
    
    /**
     * Delete user
     *
     * This can only be done by the logged in user.
     *
     */
    public void deleteUser(String username) {
        // TODO: Implement...
        
        
    }
    
    /**
     * Get user by user name
     *
     * 
     *
     */
    public User getUserByName(String username) {
        // TODO: Implement...
        
        return null;
    }
    
    /**
     * Logs user into the system
     *
     * 
     *
     */
    public String loginUser(String username, String password) {
        // TODO: Implement...
        
        return null;
    }
    
    /**
     * Logs out current logged in user session
     *
     * 
     *
     */
    public void logoutUser() {
        // TODO: Implement...
        
        
    }
    
    /**
     * Updated user
     *
     * This can only be done by the logged in user.
     *
     */
    public void updateUser(String username, User body) {
        // TODO: Implement...
        
        
    }
    
}

