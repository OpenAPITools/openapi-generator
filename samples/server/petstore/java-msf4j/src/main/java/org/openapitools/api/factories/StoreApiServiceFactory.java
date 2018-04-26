package org.openapitools.api.factories;

import org.openapitools.api.StoreApiService;
import org.openapitools.api.impl.StoreApiServiceImpl;

public class StoreApiServiceFactory {
    private final static StoreApiService service = new StoreApiServiceImpl();

    public static StoreApiService getStoreApi() {
        return service;
    }
}
