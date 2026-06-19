package org.openapitools.api;

import javax.ws.rs.ApplicationPath;
import javax.ws.rs.core.Application;

import java.util.Set;
import java.util.HashSet;

import org.openapitools.api.impl.PetApiServiceImpl;
import org.openapitools.api.impl.StoreApiServiceImpl;
import org.openapitools.api.impl.UserApiServiceImpl;

@ApplicationPath("/v2")
public class RestApplication extends Application {


    public Set<Class<?>> getClasses() {
        Set<Class<?>> resources = new HashSet<Class<?>>();
        resources.add(PetApiServiceImpl.class);
        resources.add(StoreApiServiceImpl.class);
        resources.add(UserApiServiceImpl.class);

        return resources;
    }




}