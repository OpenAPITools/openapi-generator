package org.openapitools.api.factories;

import org.openapitools.api.UserApiService;
import org.openapitools.api.impl.UserApiServiceImpl;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen", date = "2018-09-12T17:34:16.225+09:00[Asia/Tokyo]")
public class UserApiServiceFactory {
    private final static UserApiService service = new UserApiServiceImpl();

    public static UserApiService getUserApi() {
        return service;
    }
}
