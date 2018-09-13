package org.openapitools.api.factories;

import org.openapitools.api.StoreApiService;
import org.openapitools.api.impl.StoreApiServiceImpl;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen", date = "2018-08-29T07:47:48.785+02:00[Europe/Zurich]")
public class StoreApiServiceFactory {
    private final static StoreApiService service = new StoreApiServiceImpl();

    public static StoreApiService getStoreApi() {
        return service;
    }
}
