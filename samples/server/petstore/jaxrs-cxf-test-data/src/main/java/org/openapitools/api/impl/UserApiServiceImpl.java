package org.openapitools.api.impl;

import org.openapitools.api.*;
import java.util.Date;
import java.util.List;
import java.util.Map;
import org.openapitools.model.User;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import java.io.File;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.openapitools.codegen.utils.JsonCache;
import org.openapitools.codegen.utils.JsonCache.CacheException;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import org.apache.cxf.jaxrs.model.wadl.Description;
import org.apache.cxf.jaxrs.model.wadl.DocTarget;

import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.annotations.Api;

/**
 * OpenAPI Petstore
 *
 * <p>This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\
 *
 */
public class UserApiServiceImpl implements UserApi {
    private JsonCache cache;

    {
        try {
            File cacheFile = new File(System.getProperty("jaxrs.test.server.json",
                    "/Users/williamcheng/Code/openapi-generator2/samples/server/petstore/jaxrs-cxf-test-data/src/main/resources/test-data.json"));
            cache = JsonCache.Factory.instance.get("test-data").load(cacheFile).child("/org.openapitools.api/UserApi");
        } catch (CacheException e) {
            e.printStackTrace();
        }
    }

    /**
     * Create user
     *
     * This can only be done by the logged in user.
     *
     */
    @Override
    public void createUser(User body) {

    }

    /**
     * Creates list of users with given input array
     *
     */
    @Override
    public void createUsersWithArrayInput(List<User> body) {

    }

    /**
     * Creates list of users with given input array
     *
     */
    @Override
    public void createUsersWithListInput(List<User> body) {

    }

    /**
     * Delete user
     *
     * This can only be done by the logged in user.
     *
     */
    @Override
    public void deleteUser(String username) {

    }

    /**
     * Get user by user name
     *
     */
    @Override
    public User getUserByName(String username) {
        try {
            User response = cache.getObject("/getUserByName/response", User.class);
            return response;
        } catch (CacheException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Logs user into the system
     *
     */
    @Override
    public String loginUser(String username, String password) {
        try {
            String response = cache.getString("/loginUser/response");
            return response;
        } catch (CacheException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Logs out current logged in user session
     *
     */
    @Override
    public void logoutUser() {

    }

    /**
     * Updated user
     *
     * This can only be done by the logged in user.
     *
     */
    @Override
    public void updateUser(String username, User body) {

    }

}
