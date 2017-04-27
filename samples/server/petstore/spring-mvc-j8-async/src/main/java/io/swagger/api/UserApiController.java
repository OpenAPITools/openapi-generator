package io.swagger.api;

import org.springframework.stereotype.Controller;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Controller
public class UserApiController implements UserApi {
    private final ObjectMapper objectMapper;

    public UserApiController(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

}
