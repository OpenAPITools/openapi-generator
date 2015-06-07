package io.swagger.api;

import io.swagger.api.*;
import io.swagger.api.NotFoundException;
import io.swagger.model.Order;

import javax.ws.rs.core.Response;

public abstract class StoreApiService {

    public abstract Response getInventory()
            throws NotFoundException;

    public abstract Response placeOrder(Order body)
            throws NotFoundException;

    public abstract Response getOrderById(String orderId)
            throws NotFoundException;

    public abstract Response deleteOrder(String orderId)
            throws NotFoundException;

}
