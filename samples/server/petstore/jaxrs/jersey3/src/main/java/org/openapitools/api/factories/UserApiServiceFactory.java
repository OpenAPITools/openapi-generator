package org.openapitools.api.factories;

import org.openapitools.api.UserApiService;
import org.openapitools.api.impl.UserApiServiceImpl;

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen", comments = "Generator version: 7.7.0-SNAPSHOT")
public class UserApiServiceFactory {
    private static final UserApiService service = new UserApiServiceImpl();

    public static UserApiService getUserApi() {
        return service;
    }
}
