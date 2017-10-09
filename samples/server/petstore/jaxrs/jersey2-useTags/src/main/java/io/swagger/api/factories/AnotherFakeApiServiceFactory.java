package io.swagger.api.factories;

import io.swagger.api.AnotherFakeApiService;
import io.swagger.api.impl.AnotherFakeApiServiceImpl;


public class AnotherFakeApiServiceFactory {
    private final static AnotherFakeApiService service = new AnotherFakeApiServiceImpl();

    public static AnotherFakeApiService getAnotherFakeApi() {
        return service;
    }
}
