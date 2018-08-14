package org.openapitools.api.factories;

import org.openapitools.api.StoreApiService;
import org.openapitools.api.impl.StoreApiServiceImpl;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen", date = "2018-08-10T15:02:11.723+09:00[Asia/Tokyo]")
public class StoreApiServiceFactory {
    private final static StoreApiService service = new StoreApiServiceImpl();

    public static StoreApiService getStoreApi() {
        return service;
    }
}
