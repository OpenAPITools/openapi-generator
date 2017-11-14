package io.swagger.api;

import org.springframework.stereotype.Controller;

@Controller
public class UserApiController implements UserApi {

    private final UserApiDelegate delegate;

    @org.springframework.beans.factory.annotation.Autowired
    public UserApiController(UserApiDelegate delegate) {
        this.delegate = delegate;
    }

    @Override
    public UserApiDelegate getDelegate() {
        return delegate;
    }
}
