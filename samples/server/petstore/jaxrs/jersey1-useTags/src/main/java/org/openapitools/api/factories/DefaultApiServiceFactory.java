package org.openapitools.api.factories;

import org.openapitools.api.DefaultApiService;
import org.openapitools.api.impl.DefaultApiServiceImpl;


public class DefaultApiServiceFactory {
    private final static DefaultApiService service = new DefaultApiServiceImpl();

    public static DefaultApiService getDefaultApi() {
        return service;
    }
}
