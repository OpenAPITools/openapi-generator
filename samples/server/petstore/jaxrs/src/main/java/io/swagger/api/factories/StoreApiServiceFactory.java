package io.swagger.api.factories;

import io.swagger.api.StoreApiService;
import io.swagger.api.impl.StoreApiServiceImpl;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JaxRSServerCodegen", date = "2015-11-05T22:31:25.130-08:00")
public class StoreApiServiceFactory {

   private final static StoreApiService service = new StoreApiServiceImpl();

   public static StoreApiService getStoreApi()
   {
      return service;
   }
}
