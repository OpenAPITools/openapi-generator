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
import java.util.Optional;
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

    public ResponseEntity<Void> deleteOrder(@ApiParam(value = "ID of the order that needs to be deleted",required=true ) @PathVariable("order_id") String orderId,
        @RequestHeader(value = "Accept", required = false) String accept) throws Exception {
        // do some magic!
        return new ResponseEntity<Void>(HttpStatus.OK);
    }

    public ResponseEntity<Map<String, Integer>> getInventory(@RequestHeader(value = "Accept", required = false) String accept) throws Exception {
        // do some magic!

        if (accept != null && accept.contains("application/json")) {
            return new ResponseEntity<Map<String, Integer>>(objectMapper.readValue("{  \"key\" : 0}", Map.class), HttpStatus.OK);
        }

        return new ResponseEntity<Map<String, Integer>>(HttpStatus.OK);
    }

    public ResponseEntity<Order> getOrderById( @Min(1) @Max(5)@ApiParam(value = "ID of pet that needs to be fetched",required=true ) @PathVariable("order_id") Long orderId,
        @RequestHeader(value = "Accept", required = false) String accept) throws Exception {
        // do some magic!

        if (accept != null && accept.contains("application/xml")) {
            return new ResponseEntity<Order>(objectMapper.readValue("<Order>  <id>123456789</id>  <petId>123456789</petId>  <quantity>123</quantity>  <shipDate>2000-01-23T04:56:07.000Z</shipDate>  <status>aeiou</status>  <complete>true</complete></Order>", Order.class), HttpStatus.OK);
        }


        if (accept != null && accept.contains("application/json")) {
            return new ResponseEntity<Order>(objectMapper.readValue("{  \"petId\" : 6,  \"quantity\" : 1,  \"id\" : 0,  \"shipDate\" : \"2000-01-23T04:56:07.000+00:00\",  \"complete\" : false,  \"status\" : \"placed\"}", Order.class), HttpStatus.OK);
        }

        return new ResponseEntity<Order>(HttpStatus.OK);
    }

    public ResponseEntity<Order> placeOrder(@ApiParam(value = "order placed for purchasing the pet" ,required=true )  @Valid @RequestBody Order body,
        @RequestHeader(value = "Accept", required = false) String accept) throws Exception {
        // do some magic!

        if (accept != null && accept.contains("application/xml")) {
            return new ResponseEntity<Order>(objectMapper.readValue("<Order>  <id>123456789</id>  <petId>123456789</petId>  <quantity>123</quantity>  <shipDate>2000-01-23T04:56:07.000Z</shipDate>  <status>aeiou</status>  <complete>true</complete></Order>", Order.class), HttpStatus.OK);
        }


        if (accept != null && accept.contains("application/json")) {
            return new ResponseEntity<Order>(objectMapper.readValue("{  \"petId\" : 6,  \"quantity\" : 1,  \"id\" : 0,  \"shipDate\" : \"2000-01-23T04:56:07.000+00:00\",  \"complete\" : false,  \"status\" : \"placed\"}", Order.class), HttpStatus.OK);
        }

        return new ResponseEntity<Order>(HttpStatus.OK);
    }

}
