package org.openapitools.api;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.context.request.NativeWebRequest;

import jakarta.validation.constraints.*;
import java.util.Optional;
import jakarta.annotation.Generated;

/**
 * A delegate to be called by the {@link DummyApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public interface DummyApiDelegate {

    default Optional<NativeWebRequest> getRequest() {
        return Optional.empty();
    }

    /**
     * POST /dummy
     * 
     *
     * @param body  (optional)
     * @return successful operation (status code 200)
     * @see DummyApi#uploadFile
     */
    default ResponseEntity<Void> uploadFile(Optional<org.springframework.core.io.Resource> body) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

}
