package org.openapitools.api;

import org.openapitools.model.Client;
import org.springframework.web.context.request.NativeWebRequest;
import reactor.core.publisher.Mono;

import jakarta.validation.constraints.*;
import java.util.Optional;
import jakarta.annotation.Generated;

/**
 * A delegate to be called by the {@link FakeClassnameTestApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public interface FakeClassnameTestApiDelegate {

    default Optional<NativeWebRequest> getRequest() {
        return Optional.empty();
    }

    /**
     * PATCH /fake_classname_test : To test class name in snake case
     * To test class name in snake case
     *
     * @param client client model (required)
     * @return successful operation (status code 200)
     * @see FakeClassnameTestApi#testClassname
     */
    default Mono<Client> testClassname(Mono<Client> client) {
        Mono<Void> result = Mono.empty();


        return result.then(client).then(Mono.empty());

    }

}
