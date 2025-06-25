package org.openapitools.api.factories;

import org.openapitools.api.FooApiService;
import org.openapitools.api.impl.FooApiServiceImpl;

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class FooApiServiceFactory {
    private static final FooApiService service = new FooApiServiceImpl();

    public static FooApiService getFooApi() {
        return service;
    }
}
