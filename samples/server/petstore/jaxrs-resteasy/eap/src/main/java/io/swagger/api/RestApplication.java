package io.swagger.api;

import javax.ws.rs.ApplicationPath;
import javax.ws.rs.core.Application;

import java.util.Set;
import java.util.HashSet;
import io.swagger.jaxrs.config.BeanConfig;

import io.swagger.api.impl.PetApiServiceImpl;
import io.swagger.api.impl.StoreApiServiceImpl;
import io.swagger.api.impl.UserApiServiceImpl;

@ApplicationPath("/")
public class RestApplication extends Application {

	public RestApplication() {
		super();
		// Customize the dynamic contract
		BeanConfig beanConfig = new BeanConfig();
		beanConfig.setTitle("Swagger Petstore");
		beanConfig.setVersion("1.0.0");
		beanConfig.setSchemes(new String[] { "http" });
		beanConfig.setHost("petstore.swagger.io");
		beanConfig.setBasePath("/v2");
		beanConfig.setResourcePackage("io.swagger.api");
		beanConfig.setScan(true);

	}

    public Set<Class<?>> getClasses() {
        Set<Class<?>> resources = new HashSet<Class<?>>();
        resources.add(PetApiServiceImpl.class);
        resources.add(StoreApiServiceImpl.class);
        resources.add(UserApiServiceImpl.class);

        resources.add(io.swagger.jaxrs.listing.ApiListingResource.class);
        resources.add(io.swagger.jaxrs.listing.SwaggerSerializers.class);
        return resources;
    }




}