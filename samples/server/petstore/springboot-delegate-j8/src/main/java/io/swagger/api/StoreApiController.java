package io.swagger.api;

import java.util.Map;
import io.swagger.model.Order;

import io.swagger.annotations.*;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Controller
public class StoreApiController implements StoreApi {
    private final ObjectMapper objectMapper;

    public StoreApiController(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

    private final StoreApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public StoreApiController(StoreApiDelegate delegate) {
        this.delegate = delegate;
    }

    public ResponseEntity<Void> deleteOrder(@ApiParam(value = "ID of the order that needs to be deleted",required=true ) @PathVariable("order_id") String orderId,
        @RequestHeader(value = "Accept", required = false) String accept) throws Exception {
        // do some magic!
        return delegate.deleteOrder(orderId);
    }

    public ResponseEntity<Map<String, Integer>> getInventory(@RequestHeader(value = "Accept", required = false) String accept) throws Exception {
        // do some magic!
        return delegate.getInventory();
    }

    public ResponseEntity<Order> getOrderById( @Min(1) @Max(5)@ApiParam(value = "ID of pet that needs to be fetched",required=true ) @PathVariable("order_id") Long orderId,
        @RequestHeader(value = "Accept", required = false) String accept) throws Exception {
        // do some magic!
        return delegate.getOrderById(orderId);
    }

    public ResponseEntity<Order> placeOrder(@ApiParam(value = "order placed for purchasing the pet" ,required=true )  @Valid @RequestBody Order body,
        @RequestHeader(value = "Accept", required = false) String accept) throws Exception {
        // do some magic!
        return delegate.placeOrder(body);
    }

}
