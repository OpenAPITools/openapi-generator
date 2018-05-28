package org.openapitools.api;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import java.util.Optional;

@Controller
@RequestMapping("${openapi.openAPIPetstore.base-path:/v2}")
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
