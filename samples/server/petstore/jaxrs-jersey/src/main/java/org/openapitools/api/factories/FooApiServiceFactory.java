package org.openapitools.api.factories;

import org.openapitools.api.FooApiService;
import org.openapitools.api.impl.FooApiServiceImpl;


public class FooApiServiceFactory {
    private final static FooApiService service = new FooApiServiceImpl();

    public static FooApiService getFooApi() {
        return service;
    }
}
