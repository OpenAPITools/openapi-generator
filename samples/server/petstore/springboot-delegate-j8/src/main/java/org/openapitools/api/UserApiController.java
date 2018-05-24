package org.openapitools.api;

import org.springframework.stereotype.Controller;
import java.util.Optional;

@Controller
public class UserApiController implements UserApi {

    private final UserApiDelegate delegate;

    public UserApiController(@org.springframework.beans.factory.annotation.Autowired(required = false) UserApiDelegate delegate) {
        this.delegate = Optional.ofNullable(delegate).orElse(new UserApiDelegate() {});
    }

    @Override
    public UserApiDelegate getDelegate() {
        return delegate;
    }

}
