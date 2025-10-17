package org.openapitools.example.api.consumer;

import org.openapitools.example.model.Order;
import org.openapitools.example.model.*;
import org.openapitools.example.api.interfaces.StoreService;
import java.util.List;
import java.util.Map;
import java.time.OffsetDateTime;
import java.time.LocalDate;
import java.time.LocalDateTime;
import org.apache.dubbo.config.annotation.DubboReference;
import org.springframework.web.bind.annotation.*;
import javax.annotation.Generated;


@Generated(value = "org.openapitools.codegen.languages.JavaDubboServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")

@RestController
@RequestMapping("/store")
public class StoreServiceController {

    @DubboReference
    private StoreService storeService;

    @RequestMapping(method = RequestMethod.DELETE, value = "/order/{orderId}")
    public void deleteOrder(
        @RequestParam(name = "orderId") String orderId
    ) {
        storeService.deleteOrder(orderId);
    }

    @RequestMapping(method = RequestMethod.GET, value = "/inventory")
    public Map<String, Integer> getInventory(
    ) {
        return storeService.getInventory();
    }

    @RequestMapping(method = RequestMethod.GET, value = "/order/{orderId}")
    public Order getOrderById(
        @RequestParam(name = "orderId") Long orderId
    ) {
        return storeService.getOrderById(orderId);
    }

    @RequestMapping(method = RequestMethod.POST, value = "/order")
    public Order placeOrder(
        @RequestParam(name = "order") Order order
    ) {
        return storeService.placeOrder(order);
    }
}
