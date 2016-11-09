package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;

import org.wso2.msf4j.formparam.FormDataParam;
import org.wso2.msf4j.formparam.FileInfo;

import java.util.Map;
import io.swagger.model.Order;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;


public abstract class StoreApiService {
    public abstract Response deleteOrder(String orderId
 ) throws NotFoundException;
    public abstract Response getInventory() throws NotFoundException;
    public abstract Response getOrderById(Long orderId
 ) throws NotFoundException;
    public abstract Response placeOrder(Order body
 ) throws NotFoundException;
}
