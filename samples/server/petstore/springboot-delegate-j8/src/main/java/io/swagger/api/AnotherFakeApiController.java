package io.swagger.api;

import org.springframework.stereotype.Controller;

@Controller
public class AnotherFakeApiController implements AnotherFakeApi {

    private final AnotherFakeApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public AnotherFakeApiController(AnotherFakeApiDelegate delegate) {
        this.delegate = delegate;
    }

    @Override
    public AnotherFakeApiDelegate getDelegate() {
        return delegate;
    }
}
