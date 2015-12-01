package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;

import com.sun.jersey.multipart.FormDataParam;

import java.util.Map;
import io.swagger.model.Order;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import com.sun.jersey.core.header.FormDataContentDisposition;
import com.sun.jersey.multipart.FormDataParam;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JaxRSServerCodegen", date = "2015-12-01T23:25:59.622+08:00")
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
