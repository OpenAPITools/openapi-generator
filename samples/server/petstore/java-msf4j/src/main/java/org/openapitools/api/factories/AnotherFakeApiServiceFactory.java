package org.openapitools.api.factories;

import org.openapitools.api.AnotherFakeApiService;
import org.openapitools.api.impl.AnotherFakeApiServiceImpl;

public class AnotherFakeApiServiceFactory {
    private final static AnotherFakeApiService service = new AnotherFakeApiServiceImpl();

    public static AnotherFakeApiService getAnotherFakeApi() {
        return service;
    }
}
