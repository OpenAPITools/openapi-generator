package org.openapitools.api;

import org.openapitools.model.Client;
import org.springframework.web.context.request.NativeWebRequest;
import reactor.core.publisher.Mono;

import jakarta.validation.constraints.*;
import java.util.Optional;
import jakarta.annotation.Generated;

/**
 * A delegate to be called by the {@link AnotherFakeApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public interface AnotherFakeApiDelegate {

    default Optional<NativeWebRequest> getRequest() {
        return Optional.empty();
    }

    /**
     * PATCH /another-fake/dummy : To test special tags
     * To test special tags and operation ID starting with number
     *
     * @param client client model (required)
     * @return successful operation (status code 200)
     * @see AnotherFakeApi#call123testSpecialTags
     */
    default Mono<Client> call123testSpecialTags(Mono<Client> client) {
        Mono<Void> result = Mono.empty();


        return result.then(client).then(Mono.empty());

    }

}
