package io.swagger.api;

import javax.ws.rs.ApplicationPath;
import javax.ws.rs.core.Application;

import java.util.Set;
import java.util.HashSet;

import io.swagger.api.impl.PetApiServiceImpl;
import io.swagger.api.impl.StoreApiServiceImpl;
import io.swagger.api.impl.UserApiServiceImpl;

@ApplicationPath("/")
public class RestApplication extends Application {


    public Set<Class<?>> getClasses() {
        Set<Class<?>> resources = new HashSet<Class<?>>();
        resources.add(PetApiServiceImpl.class);
        resources.add(StoreApiServiceImpl.class);
        resources.add(UserApiServiceImpl.class);

        return resources;
    }




}