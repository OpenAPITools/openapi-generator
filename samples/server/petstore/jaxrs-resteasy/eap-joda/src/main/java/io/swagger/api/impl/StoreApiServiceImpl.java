package io.swagger.api.impl;

import io.swagger.api.*;
import io.swagger.model.*;


import java.util.Map;
import io.swagger.model.Order;

import java.util.List;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;


public class StoreApiServiceImpl implements StoreApi {
      public Response deleteOrder(String orderId,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response getInventory(SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response getOrderById(Long orderId,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
      public Response placeOrder(Order body,SecurityContext securityContext) {
      // do some magic!
      return Response.ok().build();
  }
}
