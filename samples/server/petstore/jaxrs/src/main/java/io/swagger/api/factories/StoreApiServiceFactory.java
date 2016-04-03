package io.swagger.api.factories;

import io.swagger.api.StoreApiService;
import io.swagger.api.impl.StoreApiServiceImpl;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaJerseyServerCodegen", date = "2016-04-02T09:18:38.134-06:00")
public class StoreApiServiceFactory {

   private final static StoreApiService service = new StoreApiServiceImpl();

   public static StoreApiService getStoreApi()
   {
      return service;
   }
}
