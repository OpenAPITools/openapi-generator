package org.openapitools.api;

import org.openapitools.api.*;
import org.openapitools.model.*;

import org.openapitools.model.Order;

import org.openapitools.api.NotFoundException;

import javax.ws.rs.core.Response;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaMSF4JServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public abstract class StoreApiService {
    public abstract Response deleteOrder(String orderId
 ) throws NotFoundException;
    public abstract Response getInventory() throws NotFoundException;
    public abstract Response getOrderById(Long orderId
 ) throws NotFoundException;
    public abstract Response placeOrder(Order body
 ) throws NotFoundException;
}
