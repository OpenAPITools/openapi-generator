package org.openapitools.api;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import java.util.Optional;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
@Controller
@RequestMapping("${openapi.openAPIPetstore.base-path:/v2}")
public class FooApiController implements FooApi {

    private final FooApiDelegate delegate;

    public FooApiController(@org.springframework.beans.factory.annotation.Autowired(required = false) FooApiDelegate delegate) {
        this.delegate = Optional.ofNullable(delegate).orElse(new FooApiDelegate() {});
    }

    @Override
    public FooApiDelegate getDelegate() {
        return delegate;
    }

}
