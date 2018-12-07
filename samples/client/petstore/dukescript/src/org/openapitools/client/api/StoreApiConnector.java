package org.openapitools.client.api;

import java.util.List;
import java.util.function.Consumer;

import org.openapitools.client.model.*;
import net.java.html.json.Model;
import net.java.html.json.ModelOperation;
import net.java.html.json.OnReceive;
import net.java.html.json.Property;

@ModelOperation
@Model(className = "StoreApi", targetId = "", properties = {
    @Property(name="url", type=String.class )
})
public class StoreApiConnector {
    // Delete purchase order by ID
    @OnReceive(method = "DELETE",  url = "{url}/store/order/{orderId}")
    public static void deleteOrder( StoreApi model, Consumer<Throwable> onError) {

    }

    // Returns pet inventories by status
    @OnReceive(method = "GET",  url = "{url}/store/inventory")
    public static void getInventory( StoreApi model, Store result,Consumer< Store> onSuccess, Consumer<Throwable> onError) {

    }

    // Find purchase order by ID
    @OnReceive(method = "GET",  url = "{url}/store/order/{orderId}")
    public static void getOrderById( StoreApi model, Store result,Consumer< Store> onSuccess, Consumer<Throwable> onError) {

    }

    // Place an order for a pet
    @OnReceive(method = "POST", data=Order.class,  url = "{url}/store/order")
    public static void placeOrder( StoreApi model, Order order, Store result,Consumer< Store> onSuccess, Consumer<Throwable> onError) {

    }

}
