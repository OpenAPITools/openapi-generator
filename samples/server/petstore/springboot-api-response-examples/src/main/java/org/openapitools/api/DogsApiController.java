package org.openapitools.api;

import org.openapitools.model.Dog;
import org.openapitools.model.Error;


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

import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import jakarta.annotation.Generated;

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
@Controller
@RequestMapping("${openapi.noExamplesInAnnotationExample.base-path:}")
public class DogsApiController implements DogsApi {

    private final DogsApiDelegate delegate;

    public DogsApiController(@Autowired(required = false) DogsApiDelegate delegate) {
        this.delegate = Optional.ofNullable(delegate).orElse(new DogsApiDelegate() {});
    }

    @Override
    public DogsApiDelegate getDelegate() {
        return delegate;
    }

}
