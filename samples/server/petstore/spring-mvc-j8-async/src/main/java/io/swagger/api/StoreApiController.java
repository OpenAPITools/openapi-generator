package io.swagger.api;

import org.springframework.stereotype.Controller;

import javax.validation.constraints.*;
import javax.validation.Valid;

@Controller
public class StoreApiController implements StoreApi {
    private final ObjectMapper objectMapper;

    public StoreApiController(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

}
