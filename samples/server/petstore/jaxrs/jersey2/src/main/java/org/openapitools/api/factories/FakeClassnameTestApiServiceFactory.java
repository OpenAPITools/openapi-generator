package org.openapitools.api.factories;

import org.openapitools.api.FakeClassnameTestApiService;
import org.openapitools.api.impl.FakeClassnameTestApiServiceImpl;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen")
public class FakeClassnameTestApiServiceFactory {
    private static final FakeClassnameTestApiService service = new FakeClassnameTestApiServiceImpl();

    public static FakeClassnameTestApiService getFakeClassnameTestApi() {
        return service;
    }
}
