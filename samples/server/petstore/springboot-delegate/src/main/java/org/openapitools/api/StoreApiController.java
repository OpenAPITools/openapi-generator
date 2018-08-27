package org.openapitools.api;

import java.util.Map;
import org.openapitools.model.Order;
import io.swagger.annotations.*;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.constraints.*;
import javax.validation.Valid;
import java.util.List;
import java.util.Map;

@Controller
@RequestMapping("${openapi.openAPIPetstore.base-path:/v2}")
public class StoreApiController implements StoreApi {

    private final StoreApiDelegate delegate;

    public StoreApiController(@org.springframework.beans.factory.annotation.Autowired(required = false) StoreApiDelegate delegate) {
        this.delegate = delegate;
    }

    public ResponseEntity<Void> deleteOrder(@ApiParam(value = "ID of the order that needs to be deleted",required=true) @PathVariable("order_id") String orderId) {
        return delegate.deleteOrder(orderId);
    }

    public ResponseEntity<Map<String, Integer>> getInventory() {
        return delegate.getInventory();
    }

    public ResponseEntity<Order> getOrderById(@Min(1L) @Max(5L) @ApiParam(value = "ID of pet that needs to be fetched",required=true) @PathVariable("order_id") Long orderId) {
        return delegate.getOrderById(orderId);
    }

    public ResponseEntity<Order> placeOrder(@ApiParam(value = "order placed for purchasing the pet" ,required=true )  @Valid @RequestBody Order order) {
        return delegate.placeOrder(order);
    }

}
