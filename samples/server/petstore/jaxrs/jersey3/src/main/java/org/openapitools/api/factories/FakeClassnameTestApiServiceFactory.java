package org.openapitools.api.factories;

import org.openapitools.api.FakeClassnameTestApiService;
import org.openapitools.api.impl.FakeClassnameTestApiServiceImpl;

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class FakeClassnameTestApiServiceFactory {
    private static final FakeClassnameTestApiService service = new FakeClassnameTestApiServiceImpl();

    public static FakeClassnameTestApiService getFakeClassnameTestApi() {
        return service;
    }
}
