package io.swagger.api.factories;

import io.swagger.api.PetApiService;
import io.swagger.api.impl.PetApiServiceImpl;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaJerseyServerCodegen", date = "2016-04-02T09:18:38.134-06:00")
public class PetApiServiceFactory {

   private final static PetApiService service = new PetApiServiceImpl();

   public static PetApiService getPetApi()
   {
      return service;
   }
}
