package io.swagger.api;

import org.springframework.stereotype.Controller;

@Controller
public class PetApiController implements PetApi {

    private final PetApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public PetApiController(PetApiDelegate delegate) {
        this.delegate = delegate;
    }

    @Override
    public PetApiDelegate getDelegate() {
        return delegate;
    }
}
