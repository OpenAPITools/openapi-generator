package org.openapitools.api;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("${openapi.openAPIPetstore.base-path:/v2}")
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
