package org.openapitools.api;

import org.openapitools.api.*;
import org.openapitools.model.*;

import org.glassfish.jersey.media.multipart.FormDataContentDisposition;

import java.util.Map;
import org.openapitools.model.Order;

import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.validation.constraints.*;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen", date = "2018-08-29T07:47:48.785+02:00[Europe/Zurich]")
public abstract class StoreApiService {
    public abstract Response deleteOrder(String orderId,SecurityContext securityContext) throws NotFoundException;
    public abstract Response getInventory(SecurityContext securityContext) throws NotFoundException;
    public abstract Response getOrderById( @Min(1L) @Max(5L)Long orderId,SecurityContext securityContext) throws NotFoundException;
    public abstract Response placeOrder(Order order,SecurityContext securityContext) throws NotFoundException;
}
