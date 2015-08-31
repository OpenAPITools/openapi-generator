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

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JaxRSServerCodegen", date = "2015-08-23T23:29:16.812-07:00")
public abstract class StoreApiService {
  
      public abstract Response getInventory()
      throws NotFoundException;
  
      public abstract Response placeOrder(Order body)
      throws NotFoundException;
  
      public abstract Response getOrderById(String orderId)
      throws NotFoundException;
  
      public abstract Response deleteOrder(String orderId)
      throws NotFoundException;
  
}
