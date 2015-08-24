package io.swagger.api.factories;

import io.swagger.api.PetApiService;
import io.swagger.api.impl.PetApiServiceImpl;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JaxRSServerCodegen", date = "2015-08-23T22:18:00.553-07:00")
public class PetApiServiceFactory {

   private final static PetApiService service = new PetApiServiceImpl();

   public static PetApiService getPetApi()
   {
      return service;
   }
}
