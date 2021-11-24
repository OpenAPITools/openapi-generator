package org.openapitools.api.factories;

import org.openapitools.api.FooApiService;
import org.openapitools.api.impl.FooApiServiceImpl;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen")
public class FooApiServiceFactory {
    private static final FooApiService service = new FooApiServiceImpl();

    public static FooApiService getFooApi() {
        return service;
    }
}
