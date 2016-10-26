package io.swagger.api.factories;

import io.swagger.api.StoreApiService;
import io.swagger.api.impl.StoreApiServiceImpl;

public class StoreApiServiceFactory {
    private final static StoreApiService service = new StoreApiServiceImpl();

    public static StoreApiService getStoreApi() {
        return service;
    }
}
