package io.swagger.api.impl;

import io.swagger.api.*;
import io.swagger.api.NotFoundException;
import io.swagger.model.Order;

import javax.ws.rs.core.Response;

public class StoreApiServiceImpl extends StoreApiService {

    @Override
    public Response getInventory()
            throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }

    @Override
    public Response placeOrder(Order body)
            throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }

    @Override
    public Response getOrderById(String orderId)
            throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }

    @Override
    public Response deleteOrder(String orderId)
            throws NotFoundException {
        // do some magic!
        return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
    }

}
