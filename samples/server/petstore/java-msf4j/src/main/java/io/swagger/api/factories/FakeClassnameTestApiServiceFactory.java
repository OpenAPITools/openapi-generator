package io.swagger.api.factories;

import io.swagger.api.FakeClassnameTestApiService;
import io.swagger.api.impl.FakeClassnameTestApiServiceImpl;

public class FakeClassnameTestApiServiceFactory {
    private final static FakeClassnameTestApiService service = new FakeClassnameTestApiServiceImpl();

    public static FakeClassnameTestApiService getFakeClassnameTestApi() {
        return service;
    }
}
