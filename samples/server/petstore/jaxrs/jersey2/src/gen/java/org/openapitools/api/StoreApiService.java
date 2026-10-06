package org.openapitools.api;

import org.openapitools.api.*;

import org.openapitools.model.Order;

import org.openapitools.api.NotFoundException;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.validation.constraints.*;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public abstract class StoreApiService {
    public abstract Response deleteOrder(String orderId,SecurityContext securityContext) throws NotFoundException;
    public abstract Response getInventory(SecurityContext securityContext) throws NotFoundException;
    public abstract Response getOrderById( @Min(1L) @Max(5L)Long orderId,SecurityContext securityContext) throws NotFoundException;
    public abstract Response placeOrder(Order body,SecurityContext securityContext) throws NotFoundException;
}
