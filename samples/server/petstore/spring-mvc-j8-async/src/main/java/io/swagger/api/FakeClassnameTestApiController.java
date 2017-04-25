package io.swagger.api;

import org.springframework.stereotype.Controller;

import javax.validation.constraints.*;
import javax.validation.Valid;

@Controller
public class FakeClassnameTestApiController implements FakeClassnameTestApi {
    private final ObjectMapper objectMapper;

    public FakeClassnameTestApiController(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

}
