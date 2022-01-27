package org.openapitools.api;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.beans.factory.annotation.Autowired;
import java.util.Optional;
import javax.annotation.Generated;

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
@Controller
@RequestMapping("${openapi.openAPIPetstore.base-path:/v2}")
public class FakeApiController implements FakeApi {

    private final FakeApiDelegate delegate;

    public FakeApiController(@Autowired(required = false) FakeApiDelegate delegate) {
        this.delegate = Optional.ofNullable(delegate).orElse(new FakeApiDelegate() {});
    }

    @Override
    public FakeApiDelegate getDelegate() {
        return delegate;
    }

}
