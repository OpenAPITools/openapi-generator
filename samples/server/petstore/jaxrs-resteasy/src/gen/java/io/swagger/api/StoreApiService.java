package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;


import java.util.Map;
import io.swagger.model.Order;


import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaResteasyServerCodegen", date = "2016-02-04T01:58:20.368+07:00")

public abstract class StoreApiService {
  
      public abstract Response getInventory(SecurityContext securityContext)
      throws NotFoundException;
  
      public abstract Response placeOrder(Order body,SecurityContext securityContext)
      throws NotFoundException;
  
      public abstract Response getOrderById(String orderId,SecurityContext securityContext)
      throws NotFoundException;
  
      public abstract Response deleteOrder(String orderId,SecurityContext securityContext)
      throws NotFoundException;
  
}

