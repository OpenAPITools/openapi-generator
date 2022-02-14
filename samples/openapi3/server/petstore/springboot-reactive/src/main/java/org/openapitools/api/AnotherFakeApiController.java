package org.openapitools.api;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.beans.factory.annotation.Autowired;
import java.util.Optional;
import javax.annotation.Generated;

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
@Controller
@RequestMapping("${openapi.openAPIPetstore.base-path:/v2}")
public class AnotherFakeApiController implements AnotherFakeApi {

    private final AnotherFakeApiDelegate delegate;

    public AnotherFakeApiController(@Autowired(required = false) AnotherFakeApiDelegate delegate) {
        this.delegate = Optional.ofNullable(delegate).orElse(new AnotherFakeApiDelegate() {});
    }

    @Override
    public AnotherFakeApiDelegate getDelegate() {
        return delegate;
    }

    /**
     * PATCH /another-fake/dummy : To test special tags
     * To test special tags and operation ID starting with number
     *
     * @param body client model (required)
     * @return successful operation (status code 200)
     * @see AnotherFakeApi#call123testSpecialTags
     */
    public Mono<ResponseEntity<Client>> call123testSpecialTags(
        @Parameter(name = "body", description = "client model", required = true) @Valid @RequestBody Mono<Client> body
    ) {
        return delegate.call123testSpecialTags(body);
    }

}
