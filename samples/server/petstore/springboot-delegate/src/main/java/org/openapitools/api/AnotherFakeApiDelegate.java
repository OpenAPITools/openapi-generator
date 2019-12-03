package org.openapitools.api;

import org.openapitools.model.Client;
import io.swagger.annotations.*;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;

/**
 * A delegate to be called by the {@link AnotherFakeApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */

public interface AnotherFakeApiDelegate {

    /**
     * PATCH /another-fake/dummy : To test special tags
     * To test special tags and operation ID starting with number
     *
     * @param body client model (required)
     * @return the response
     * @see AnotherFakeApi#call123testSpecialTags
     */
    ResponseEntity<Client> call123testSpecialTags(Client body);

}
