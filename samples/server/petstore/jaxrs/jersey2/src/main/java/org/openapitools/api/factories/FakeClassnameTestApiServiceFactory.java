package org.openapitools.api.factories;

import org.openapitools.api.FakeClassnameTestApiService;
import org.openapitools.api.impl.FakeClassnameTestApiServiceImpl;


public class FakeClassnameTestApiServiceFactory {
    private final static FakeClassnameTestApiService service = new FakeClassnameTestApiServiceImpl();

    public static FakeClassnameTestApiService getFakeClassnameTestApi() {
        return service;
    }
}
