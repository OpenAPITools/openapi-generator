package org.openapitools.api;

import org.openapitools.api.*;
import org.openapitools.model.*;

import org.openapitools.model.Order;

import org.openapitools.api.NotFoundException;

import javax.validation.constraints.*;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaResteasyServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public interface StoreApiService {
      Response deleteOrder(String orderId,SecurityContext securityContext)
      throws NotFoundException;
      Response getInventory(SecurityContext securityContext)
      throws NotFoundException;
      Response getOrderById(Long orderId,SecurityContext securityContext)
      throws NotFoundException;
      Response placeOrder(Order body,SecurityContext securityContext)
      throws NotFoundException;


}
