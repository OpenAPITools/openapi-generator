package io.swagger.api;

import org.springframework.stereotype.Controller;

@Controller
public class StoreApiController implements StoreApi {

    private final StoreApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public StoreApiController(StoreApiDelegate delegate) {
        this.delegate = delegate;
    }

    @Override
    public StoreApiDelegate getDelegate() {
        return delegate;
    }
}
