package org.openapitools.server.api;

import io.helidon.webserver.Handler;
import java.util.Map;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.openapitools.server.model.Order;

import java.util.Optional;
import java.util.logging.Logger;

import io.helidon.common.GenericType;
import io.helidon.common.reactive.Single;
import io.helidon.config.Config;
import io.helidon.webserver.Routing;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;
import io.helidon.webserver.Service;


public abstract class StoreService implements Service { 

    protected static final Logger LOGGER = Logger.getLogger(StoreService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();

    protected final Config config;

    public StoreService(Config config) {
        this.config = config;
    }

    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    public void update(Routing.Rules rules) {
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
    void deleteOrder(ServerRequest request, ServerResponse response) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                String orderId = Optional.ofNullable(request.path().param("order_id")).orElse(null);
                ValidatorUtils.checkNonNull(orderId);
                
                handleDeleteOrder(request, response, orderId);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle DELETE /store/order/{order_id} : Delete purchase order by ID.
     * @param request the server request
     * @param response the server response
     * @param orderId ID of the order that needs to be deleted 
     */
    abstract void handleDeleteOrder(ServerRequest request, ServerResponse response, String orderId);


    /**
     * GET /store/inventory : Returns pet inventories by status.
     * @param request the server request
     * @param response the server response
     */
    void getInventory(ServerRequest request, ServerResponse response) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                
                handleGetInventory(request, response);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle GET /store/inventory : Returns pet inventories by status.
     * @param request the server request
     * @param response the server response
     */
    abstract void handleGetInventory(ServerRequest request, ServerResponse response);


    /**
     * GET /store/order/{order_id} : Find purchase order by ID.
     * @param request the server request
     * @param response the server response
     */
    void getOrderById(ServerRequest request, ServerResponse response) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                Long orderId = Optional.ofNullable(request.path().param("order_id")).map(Long::valueOf).orElse(null);
                ValidatorUtils.checkNonNull(orderId);
                ValidatorUtils.validateMin(orderId.intValue(), 1);
                ValidatorUtils.validateMax(orderId.intValue(), 5);
                
                handleGetOrderById(request, response, orderId);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle GET /store/order/{order_id} : Find purchase order by ID.
     * @param request the server request
     * @param response the server response
     * @param orderId ID of pet that needs to be fetched 
     */
    abstract void handleGetOrderById(ServerRequest request, ServerResponse response, Long orderId);


    /**
     * POST /store/order : Place an order for a pet.
     * @param request the server request
     * @param response the server response
     * @param order order placed for purchasing the pet 
     */
    void placeOrder(ServerRequest request, ServerResponse response, Order order) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                ValidatorUtils.checkNonNull(order);
                
                handlePlaceOrder(request, response, order);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle POST /store/order : Place an order for a pet.
     * @param request the server request
     * @param response the server response
     * @param order order placed for purchasing the pet 
     */
    abstract void handlePlaceOrder(ServerRequest request, ServerResponse response, Order order);


    abstract Void handleError(ServerRequest request, ServerResponse response, Throwable throwable);
}
