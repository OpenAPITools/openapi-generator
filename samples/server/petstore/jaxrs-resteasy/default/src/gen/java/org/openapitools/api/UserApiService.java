package org.openapitools.api;

import org.openapitools.api.*;
import org.openapitools.model.*;


import java.util.Date;
import org.openapitools.model.User;

import java.util.List;
import org.openapitools.api.NotFoundException;
import java.io.InputStream;

import javax.validation.constraints.*;
import javax.validation.Valid;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaResteasyServerCodegen", comments = "Generator version: 7.18.0-SNAPSHOT")
public interface UserApiService {
    Response createUser(
        
        User body,
        SecurityContext securityContext
    ) throws NotFoundException;

    default Response createUser(
        
        User body, 
        String context
    ) throws NotFoundException {
        return createUser(
            
            body,
            (SecurityContext)null
        );
    }
    Response createUsersWithArrayInput(
        
        List<@Valid User> body,
        SecurityContext securityContext
    ) throws NotFoundException;

    default Response createUsersWithArrayInput(
        
        List<@Valid User> body, 
        String context
    ) throws NotFoundException {
        return createUsersWithArrayInput(
            
            body,
            (SecurityContext)null
        );
    }
    Response createUsersWithListInput(
        
        List<@Valid User> body,
        SecurityContext securityContext
    ) throws NotFoundException;

    default Response createUsersWithListInput(
        
        List<@Valid User> body, 
        String context
    ) throws NotFoundException {
        return createUsersWithListInput(
            
            body,
            (SecurityContext)null
        );
    }
    Response deleteUser(
        
        String username,
        SecurityContext securityContext
    ) throws NotFoundException;

    default Response deleteUser(
        
        String username, 
        String context
    ) throws NotFoundException {
        return deleteUser(
            
            username,
            (SecurityContext)null
        );
    }
    Response getUserByName(
        
        String username,
        SecurityContext securityContext
    ) throws NotFoundException;

    default Response getUserByName(
        
        String username, 
        String context
    ) throws NotFoundException {
        return getUserByName(
            
            username,
            (SecurityContext)null
        );
    }
    Response loginUser(
        
        String username,
        String password,
        SecurityContext securityContext
    ) throws NotFoundException;

    default Response loginUser(
        
        String username, 
        String password, 
        String context
    ) throws NotFoundException {
        return loginUser(
            
            username,password,
            (SecurityContext)null
        );
    }
    Response logoutUser(
        
        SecurityContext securityContext
    ) throws NotFoundException;

    default Response logoutUser(
        
        String context
    ) throws NotFoundException {
        return logoutUser(
            
            
            (SecurityContext)null
        );
    }
    Response updateUser(
        
        String username,
        User body,
        SecurityContext securityContext
    ) throws NotFoundException;

    default Response updateUser(
        
        String username, 
        User body, 
        String context
    ) throws NotFoundException {
        return updateUser(
            
            username,body,
            (SecurityContext)null
        );
    }
}
