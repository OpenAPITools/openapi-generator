package io.swagger.api;

import org.springframework.stereotype.Controller;

@Controller
public class FakeClassnameTestApiController implements FakeClassnameTestApi {

    private final FakeClassnameTestApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public FakeClassnameTestApiController(FakeClassnameTestApiDelegate delegate) {
        this.delegate = delegate;
    }

    @Override
    public FakeClassnameTestApiDelegate getDelegate() {
        return delegate;
    }
}
