package org.openapitools.api;

import org.openapitools.api.*;
import org.openapitools.model.*;

import org.wso2.msf4j.formparam.FormDataParam;
import org.wso2.msf4j.formparam.FileInfo;

import java.util.Map;
import org.openapitools.model.Order;

import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaMSF4JServerCodegen")
public abstract class StoreApiService {
    public abstract Response deleteOrder(String orderId
 ) throws NotFoundException;
    public abstract Response getInventory() throws NotFoundException;
    public abstract Response getOrderById(Long orderId
 ) throws NotFoundException;
    public abstract Response placeOrder(Order body
 ) throws NotFoundException;
}
