package org.openapitools.api.factories;

import org.openapitools.api.PetApiService;
import org.openapitools.api.impl.PetApiServiceImpl;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen", date = "2018-08-10T15:02:11.723+09:00[Asia/Tokyo]")
public class PetApiServiceFactory {
    private final static PetApiService service = new PetApiServiceImpl();

    public static PetApiService getPetApi() {
        return service;
    }
}
