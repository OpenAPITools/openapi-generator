package org.openapitools.api.factories;

import org.openapitools.api.PetApiService;
import org.openapitools.api.impl.PetApiServiceImpl;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class PetApiServiceFactory {
    private static final PetApiService service = new PetApiServiceImpl();

    public static PetApiService getPetApi() {
        return service;
    }
}
