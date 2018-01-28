package io.swagger.api;

import io.swagger.model.Client;
import io.swagger.annotations.*;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

/**
 * A delegate to be called by the {@link AnotherFakeApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */

public interface AnotherFakeApiDelegate {

    /**
     * @see AnotherFakeApi#testSpecialTags
     */
    ResponseEntity<Client> testSpecialTags( Client  body);

}
