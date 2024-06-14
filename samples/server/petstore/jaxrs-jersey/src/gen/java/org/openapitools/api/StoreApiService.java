package org.openapitools.api;

import org.openapitools.api.*;

import org.glassfish.jersey.media.multipart.FormDataBodyPart;

import java.util.Map;
import org.openapitools.model.Order;

import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.validation.constraints.*;
import javax.validation.Valid;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen", comments = "Generator version: 7.7.0-SNAPSHOT")
public abstract class StoreApiService {
    public abstract Response deleteOrder(String orderId,SecurityContext securityContext) throws NotFoundException;
    public abstract Response getInventory(SecurityContext securityContext) throws NotFoundException;
    public abstract Response getOrderById( @Min(1L) @Max(5L)Long orderId,SecurityContext securityContext) throws NotFoundException;
    public abstract Response placeOrder(Order order,SecurityContext securityContext) throws NotFoundException;
}
