package io.swagger.api;

import io.swagger.model.Client;
import io.swagger.annotations.*;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

/**
 * A delegate to be called by the {@link FakeClassnameTestApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */

public interface FakeClassnameTestApiDelegate {

    /**
     * @see FakeClassnameTestApi#testClassname
     */
    ResponseEntity<Client> testClassname( Client  body);

}
