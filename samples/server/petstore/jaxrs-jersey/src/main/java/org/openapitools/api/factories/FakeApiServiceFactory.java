package org.openapitools.api.factories;

import org.openapitools.api.FakeApiService;
import org.openapitools.api.impl.FakeApiServiceImpl;


public class FakeApiServiceFactory {
    private final static FakeApiService service = new FakeApiServiceImpl();

    public static FakeApiService getFakeApi() {
        return service;
    }
}
