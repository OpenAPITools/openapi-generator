package org.openapitools.api;

import org.openapitools.api.*;
import org.openapitools.model.*;

import com.sun.jersey.multipart.FormDataParam;

import java.util.Map;
import org.openapitools.model.Order;

import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import com.sun.jersey.multipart.FormDataParam;
import com.sun.jersey.multipart.FormDataBodyPart;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.validation.constraints.*;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen")
public abstract class StoreApiService {
      public abstract Response deleteOrder(String orderId,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response getInventory(SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response getOrderById( @Min(1L) @Max(5L)Long orderId,SecurityContext securityContext)
      throws NotFoundException;
      public abstract Response placeOrder(Order body,SecurityContext securityContext)
      throws NotFoundException;
}
