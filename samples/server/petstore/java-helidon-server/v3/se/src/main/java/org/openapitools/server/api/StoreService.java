package org.openapitools.server.api;

import io.helidon.webserver.Handler;
import java.util.Map;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.openapitools.server.model.Order;

import io.helidon.webserver.Routing;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;
import io.helidon.webserver.Service;

public interface StoreService extends Service { 

    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    default void update(Routing.Rules rules) {
        rules.delete("/store/order/{order_id}", this::deleteOrder);
        rules.get("/store/inventory", this::getInventory);
        rules.get("/store/order/{order_id}", this::getOrderById);
        rules.post("/store/order", Handler.create(Order.class, this::placeOrder));
    }


    /**
     * DELETE /store/order/{order_id} : Delete purchase order by ID.
     * @param request the server request
     * @param response the server response
     */
    void deleteOrder(ServerRequest request, ServerResponse response);

    /**
     * GET /store/inventory : Returns pet inventories by status.
     * @param request the server request
     * @param response the server response
     */
    void getInventory(ServerRequest request, ServerResponse response);

    /**
     * GET /store/order/{order_id} : Find purchase order by ID.
     * @param request the server request
     * @param response the server response
     */
    void getOrderById(ServerRequest request, ServerResponse response);

    /**
     * POST /store/order : Place an order for a pet.
     * @param request the server request
     * @param response the server response
     * @param order order placed for purchasing the pet 
     */
    void placeOrder(ServerRequest request, ServerResponse response, Order order);

}
