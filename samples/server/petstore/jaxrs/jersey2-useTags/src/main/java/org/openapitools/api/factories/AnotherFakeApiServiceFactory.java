package org.openapitools.api.factories;

import org.openapitools.api.AnotherFakeApiService;
import org.openapitools.api.impl.AnotherFakeApiServiceImpl;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class AnotherFakeApiServiceFactory {
    private static final AnotherFakeApiService service = new AnotherFakeApiServiceImpl();

    public static AnotherFakeApiService getAnotherFakeApi() {
        return service;
    }
}
