package org.openapitools.codegen.online.api;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

@Controller
public class GenApiController implements GenApi {

    private final GenApiDelegate delegate;

    @Autowired
    public GenApiController(GenApiDelegate delegate) {
        this.delegate = delegate;
    }

    @Override
    public GenApiDelegate getDelegate() {
        return delegate;
    }
}
