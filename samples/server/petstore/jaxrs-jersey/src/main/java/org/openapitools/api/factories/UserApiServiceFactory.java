package org.openapitools.api.factories;

import org.openapitools.api.UserApiService;
import org.openapitools.api.impl.UserApiServiceImpl;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen", date = "2018-09-28T11:11:41.612+08:00[Asia/Taipei]")
public class UserApiServiceFactory {
    private final static UserApiService service = new UserApiServiceImpl();

    public static UserApiService getUserApi() {
        return service;
    }
}
