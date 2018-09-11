package org.openapitools.api.factories;

import org.openapitools.api.PetApiService;
import org.openapitools.api.impl.PetApiServiceImpl;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen", date = "2018-09-11T15:41:15.984+09:00[Asia/Tokyo]")
public class PetApiServiceFactory {
    private final static PetApiService service = new PetApiServiceImpl();

    public static PetApiService getPetApi() {
        return service;
    }
}
