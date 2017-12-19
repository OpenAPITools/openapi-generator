package com.prokarma.pkmst.controller;

import java.util.Map;
import com.prokarma.pkmst.model.Order;

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
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
/**
 * Api implemention
 * @author pkmst
 *
 */

@Controller
public class StoreApiController implements StoreApi {
    private final ObjectMapper objectMapper;
@Autowired
    public StoreApiController(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

    public ResponseEntity<Void> deleteOrder(@ApiParam(value = "ID of the order that needs to be deleted",required=true ) @PathVariable("orderId") String orderId,
        @RequestHeader(value = "Accept", required = false) String accept) throws Exception {
        // do some magic!
        return new ResponseEntity<Void>(HttpStatus.OK);
    }

    public ResponseEntity<Map<String, Integer>> getInventory(@RequestHeader(value = "Accept", required = false) String accept) throws Exception {
        // do some magic!

        if (accept != null && accept.contains("application/json")) {
            return new ResponseEntity<Map<String, Integer>>(objectMapper.readValue("", Map.class), HttpStatus.OK);
        }

        return new ResponseEntity<Map<String, Integer>>(HttpStatus.OK);
    }

    public ResponseEntity<Order> getOrderById(@ApiParam(value = "ID of pet that needs to be fetched",required=true ) @PathVariable("orderId") Long orderId,
        @RequestHeader(value = "Accept", required = false) String accept) throws Exception {
        // do some magic!

        if (accept != null && accept.contains("application/xml")) {
            return new ResponseEntity<Order>(objectMapper.readValue("", Order.class), HttpStatus.OK);
        }


        if (accept != null && accept.contains("application/json")) {
            return new ResponseEntity<Order>(objectMapper.readValue("", Order.class), HttpStatus.OK);
        }

        return new ResponseEntity<Order>(HttpStatus.OK);
    }

    public ResponseEntity<Order> placeOrder(@ApiParam(value = "order placed for purchasing the pet" ,required=true )   @RequestBody Order body,
        @RequestHeader(value = "Accept", required = false) String accept) throws Exception {
        // do some magic!

        if (accept != null && accept.contains("application/xml")) {
            return new ResponseEntity<Order>(objectMapper.readValue("", Order.class), HttpStatus.OK);
        }


        if (accept != null && accept.contains("application/json")) {
            return new ResponseEntity<Order>(objectMapper.readValue("", Order.class), HttpStatus.OK);
        }

        return new ResponseEntity<Order>(HttpStatus.OK);
    }

}
