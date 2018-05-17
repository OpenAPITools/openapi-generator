package org.openapitools.api;

import org.springframework.stereotype.Controller;
import java.util.Optional;

@Controller
public class FakeApiController implements FakeApi {

    private final FakeApiDelegate delegate;

    public FakeApiController(@org.springframework.beans.factory.annotation.Autowired(required = false) FakeApiDelegate delegate) {
        this.delegate = Optional.ofNullable(delegate).orElse(new FakeApiDelegate() {});
    }

    @Override
    public FakeApiDelegate getDelegate() {
        return delegate;
    }

}
