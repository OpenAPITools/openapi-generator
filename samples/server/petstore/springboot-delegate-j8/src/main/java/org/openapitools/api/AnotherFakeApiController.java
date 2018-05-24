package org.openapitools.api;

import org.springframework.stereotype.Controller;
import java.util.Optional;

@Controller
public class AnotherFakeApiController implements AnotherFakeApi {

    private final AnotherFakeApiDelegate delegate;

    public AnotherFakeApiController(@org.springframework.beans.factory.annotation.Autowired(required = false) AnotherFakeApiDelegate delegate) {
        this.delegate = Optional.ofNullable(delegate).orElse(new AnotherFakeApiDelegate() {});
    }

    @Override
    public AnotherFakeApiDelegate getDelegate() {
        return delegate;
    }

}
