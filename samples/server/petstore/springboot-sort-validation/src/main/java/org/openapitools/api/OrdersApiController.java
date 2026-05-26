package org.openapitools.api;

import org.springframework.lang.Nullable;
import org.openapitools.model.OrderStatus;


import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.context.request.NativeWebRequest;

import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import jakarta.annotation.Generated;

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.23.0-SNAPSHOT")
@Controller
public class OrdersApiController implements OrdersApi {

    @Override
    public ResponseEntity<Void> listOrders(
            @Valid @Nullable OrderStatus filter,
            @Valid @Nullable OrderStatus orderStatus,
            @Valid @Nullable OrderStatus itemStatus) {
        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Void> listOrdersFilterOnly(@Valid @Nullable OrderStatus filter) {
        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Void> listOrdersOrderStatusOnly(@Valid @Nullable OrderStatus orderStatus) {
        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Void> listOrdersItemStatusOnly(@Valid @Nullable OrderStatus itemStatus) {
        return new ResponseEntity<>(HttpStatus.OK);
    }

}
