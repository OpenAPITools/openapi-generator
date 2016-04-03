package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;

import org.glassfish.jersey.media.multipart.FormDataContentDisposition;

import java.util.Map;
import io.swagger.model.Order;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaJerseyServerCodegen", date = "2016-04-02T09:12:46.799-06:00")
public abstract class StoreApiService {
    
    public abstract Response deleteOrder(Long orderId,SecurityContext securityContext) throws NotFoundException;
    
    public abstract Response getInventory(SecurityContext securityContext) throws NotFoundException;
    
    public abstract Response getOrderById(Long orderId,SecurityContext securityContext) throws NotFoundException;
    
    public abstract Response placeOrder(Order body,SecurityContext securityContext) throws NotFoundException;
    
}
