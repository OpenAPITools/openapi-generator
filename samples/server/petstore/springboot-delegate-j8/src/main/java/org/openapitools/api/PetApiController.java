package org.openapitools.api;

import org.springframework.stereotype.Controller;
import java.util.Optional;

@Controller
public class PetApiController implements PetApi {

    private final PetApiDelegate delegate;

    public PetApiController(@org.springframework.beans.factory.annotation.Autowired(required = false) PetApiDelegate delegate) {
        this.delegate = Optional.ofNullable(delegate).orElse(new PetApiDelegate() {});
    }

    @Override
    public PetApiDelegate getDelegate() {
        return delegate;
    }

}
