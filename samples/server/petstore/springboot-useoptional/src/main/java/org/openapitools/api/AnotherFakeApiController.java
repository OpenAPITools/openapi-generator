package org.openapitools.api;

import org.springframework.stereotype.Controller;
import org.springframework.web.context.request.NativeWebRequest;
import java.util.Optional;

@Controller
public class AnotherFakeApiController implements AnotherFakeApi {

    private final NativeWebRequest request;

    @org.springframework.beans.factory.annotation.Autowired
    public AnotherFakeApiController(NativeWebRequest request) {
        this.request = request;
    }

    @Override
    public Optional<NativeWebRequest> getRequest() {
        return Optional.ofNullable(request);
    }

}
