package io.swagger.api.factories;

import io.swagger.api.FakeApiService;
import io.swagger.api.impl.FakeApiServiceImpl;

public class FakeApiServiceFactory {
    private final static FakeApiService service = new FakeApiServiceImpl();

    public static FakeApiService getFakeApi() {
        return service;
    }
}
