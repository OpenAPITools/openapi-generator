package org.openapitools.api;

import org.openapitools.model.InlineResponseDefault;
import io.swagger.annotations.*;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;

/**
 * A delegate to be called by the {@link FooApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public interface FooApiDelegate {

    /**
     * GET /foo
     *
     * @return response (status code 200)
     * @see FooApi#fooGet
     */
    ResponseEntity<InlineResponseDefault> fooGet();

}
