package io.swagger.api;

import io.swagger.model.Client;

import io.swagger.annotations.*;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

/**
 * A delegate to be called by the {@link FakeClassnameTestApiController}}.
 * Should be implemented as a controller but without the {@link org.springframework.stereotype.Controller} annotation.
 * Instead, use spring to autowire this class into the {@link FakeClassnameTestApiController}.
 */

public interface FakeClassnameTestApiDelegate {

    /**
     * @see FakeClassnameTestApi#testClassname
     */
    ResponseEntity<Client> testClassname(Client body);

}
